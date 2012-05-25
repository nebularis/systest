%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com).
%%
%% Some portions of the code taken from sh (c) 2005 - 2012 Nebularis
%% Some portions of the code taken from rebar (c) 2010 Dave Smith
%% Some portions of the code taken from retest (c) 2010 Dave Smith
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------
-module(systest_cli).

-behaviour(systest_node).
-behaviour(gen_server).

%% API Exports

-export([start/1, start_link/1, stop/1, kill/1]).
-export([status/1, interact/2]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% private record for tracking...
-record(sh, {command, node, state, log_enabled,
             rpc_enabled, pid, port, shutdown}).

-include("systest.hrl").

-ifdef(TEST).
-export([convert_flags/4]).
-endif.

-spec start(systest_node:node_info()) -> systest_node:node_info() | term().
start(NodeInfo) ->
    start_it(NodeInfo, start).

-spec start_link(systest_node:node_info()) ->
                            systest_node:node_info() | term().
start_link(NodeInfo) ->
    start_it(NodeInfo, start_link).

-spec stop(systest_node:node_info()) -> 'ok'.
stop(#'systest.node_info'{owner=Server}) ->
    gen_server:cast(Server, stop).

-spec kill(systest_node:node_info()) -> 'ok'.
kill(#'systest.node_info'{owner=Server}) ->
    gen_server:cast(Server, kill).

-spec status(systest_node:node_info()) -> 'nodeup' | {'nodedown', term()}.
status(#'systest.node_info'{owner=Server}) ->
    gen_server:call(Server, ping).

-spec interact(systest_node:node_info(),
               {module(), atom(), [term()]} | string()) -> term().
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}) ->
    rpc:call(Node, Mod, Func, Args);
interact(#'systest.node_info'{owner=Server}, Data) ->
    gen_server:call(Server, {command, Data}).

%%
%% OTP gen_server API
%%

init([Node, Cmd, Args, Extra]) ->
    Id = systest_node:get_node_info(id, Node),
    Host = systest_node:get_node_info(host, Node),
    Config = systest_node:get_node_info(config, Node),
    Startup = ?CONFIG(startup, Config, true),
    LogEnabled = ?CONFIG(log_enabled, Startup, true),
    {RpcEnabled, ShutdownSpec} = ?CONFIG(rpc_enabled,
                                         Startup, {true, default}),
    case check_command(Cmd, RpcEnabled) of
        ok ->
            Env = ?CONFIG(env, Extra, []),
            ExecutableCommand = maybe_patch_command(Cmd, Env, RpcEnabled),
            Port = open_port({spawn_executable, ExecutableCommand},
                             [{args, Args}, exit_status, hide,
                              stderr_to_stdout, use_stdio, {line, 16384}]),
            %% we do the initial receive stuff up-front
            %% just to avoid any initial ordering problems...
            case read_pid(Id, Port, RpcEnabled) of
                {error, {stopped, Rc}} ->
                    {stop, {launch_failure, Rc}};
                {error, Reason}=Err ->
                    {stop, {launch_failure, Reason}};
                Pid ->
                    Sh = #sh{pid=Pid,
                             port=Port,
                             log_enabled=LogEnabled,
                             rpc_enabled=RpcEnabled,
                             shutdown=ShutdownSpec,
                             command=ExecutableCommand,
                             state=running,
                             node=Node#'systest.node_info'{os_pid=Pid}},
                    ct:log(info,
                           "External Process Handler Started at ~p:~n~p~n",
                           [self(), Sh]),
                    {ok, Sh}
            end;
        StopError ->
            StopError
    end.

handle_call(os_pid, _From, Sh=#sh{pid=Pid}) ->
    {reply, Pid, Sh};
handle_call({command, Data}, _From, Sh=#sh{port=Port}) ->
    port_command(Port, Data, [nosuspend]),
    {reply, ok, Sh};
handle_call(ping, _From, Sh=#sh{rpc_enabled=true, node=Node}) ->
    {reply, systest_node:status_check(Node#'systest.node_info'.id), Sh};
handle_call(ping, _From, Sh=#sh{rpc_enabled=false, state=ProgramState}) ->
    case ProgramState of
        running -> {reply, nodeup, Sh};
        stopped -> {reply, {nodedown, stopped}, Sh}
    end;
handle_call(_Msg, _From, Sh) ->
    {noreply, Sh}.

handle_cast(kill, Sh=#sh{state=stopped}) ->
    {noreply, Sh};
handle_cast(kill, Sh=#sh{pid=Pid, rpc_enabled=false, state=running}) ->
    _ = os:cmd("kill -9 " ++ Pid),
    {noreply, Sh#sh{state=stopped}};
handle_cast(kill, Sh=#sh{port=Port, rpc_enabled=true, state=running}) ->
    erlang:port_close(Port),
    {stop, killed, Sh#sh{state=stopped}};
handle_cast(stop, Sh=#sh{node=Node, shutdown=Shutdown, rpc_enabled=true}) ->
    Halt = case Shutdown of
               default -> {init, stop, []};
               Custom  -> Custom
           end,
    apply(rpc, call, [Node#'systest.node_info'.id|tuple_to_list(Halt)]),
    {noreply, Sh#sh{state=stopped}};
handle_cast(_Msg, Sh) ->
    {noreply, Sh}.

handle_info({Port, {data, {_, Line}}},
            Sh=#sh{port=Port, node=Node, log_enabled=true}) ->
    ct:log("[~p] " ++ Line, [systest_node:get_node_info(id, Node)]),
    {noreply, Sh};
handle_info({Port, {exit_status, 0}},
            Sh=#sh{port=Port, log_enabled=Pal, command=Cmd}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["Program ~s exited normally (status 0)~n", [Cmd]]),
    {stop, normal, Sh#sh{state=stopped}};
handle_info({Port, {exit_status, Exit}=Rc},
             Sh=#sh{port=Port, log_enabled=Pal, node=Node}) ->
     LogFun = case Pal of true -> pal; _ -> log end,
     apply(ct, LogFun, ["Node ~p shut down with error/status code ~p~n",
                        [Node#'systest.node_info'.id, Exit]]),
    {stop, Rc, Sh#sh{state=stopped}};
handle_info(_Info, Sh) ->
    {noreply, Sh}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

start_it(NI=#'systest.node_info'{config=Config, flags=Flags,
                                 host=Host, name=Name}, StartType) ->
    Id = list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)),
    NI2 = systest_node:set_node_info([{id, Id}], NI),
    {Env, Args, Prog} = convert_flags(start, NI2, Flags, Config),
    Extra = [{env, Env}|?CONFIG(extra, Config, [])],
    case apply(gen_server, StartType,
              [?MODULE, [NI2, Prog, Args, Extra], []]) of
        {ok, Pid} -> OsPid = gen_server:call(Pid, os_pid),
                             NI2#'systest.node_info'{os_pid=OsPid, owner=Pid};
        Error     -> Error
    end.

%% port handling

read_pid(NodeId, Port, RpcEnabled) ->
    case RpcEnabled of
        true  -> case rpc:call(NodeId, os, getpid, []) of
                     {badrpc, Reason} ->
                         receive
                             {Port, {exit_status, Rc}} ->
                                 {error, {stopped, Rc}};
                             Other ->
                                 ct:log("Unexpected data from ~p: ~p~n",
                                        [NodeId, Other]),
                                 read_pid(NodeId, Port, RpcEnabled)
                         after 5000 ->
                             read_pid(NodeId, Port, RpcEnabled)
                         end;
                     Pid ->
                        Pid
                 end;
        false -> receive
                     {Port, {data, {eol, Pid}}} ->
                         Pid;
                     {Port, {exit_status, Rc}} ->
                         {error, {stopped, Rc}}
                 end
    end.

%% command processing

check_command(_, true) ->
    ok;
check_command(Cmd, _) ->
    case re:run(Cmd, "(&&|;)") of
        nomatch -> ok;
        _       -> {stop, async_multicmds_disallowed}
    end.

maybe_patch_command(Cmd, _, true) ->
    Cmd;
maybe_patch_command(Cmd, Env, false) ->
    case os:type() of
        {win32, _} ->
            "cmd /q /c " ++ lists:foldl(fun({Key, Value}, Acc) ->
                                        expand_env_variable(Acc, Key, Value)
                                        end, Cmd, Env);
        _ ->
            "/usr/bin/env sh -c \"echo $$; exec " ++ Cmd ++ "\""
    end.

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", [global]),
            %% Use a regex to match/replace:
            %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
            RegEx = io_lib:format("\\\$(~s(\\s|$)|{~s})", [VarName, VarName]),
            ReOpts = [global, {return, list}],
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

%% node configuration/setup

convert_flags(Operation, Node, AllFlags, Config) ->
    Flags = ?REQUIRE(Operation, AllFlags),
    {_, _, Env, Acc, Prog} =
            lists:foldl(fun process/2,
                        {Node, Config, [], [], undefined}, Flags),
    {Env, Acc, Prog}.

process({program, Prog}, Acc) ->
    setelement(5, Acc, Prog);
process({node, Attr}, {Node, _, _, Output, _}=Acc) ->
    setelement(4, Acc, Output ++
                       [atom_to_list(systest_node:get_node_info(Attr, Node))]);
process({environment, Attr}, {_, _, Output, _, _}=Acc) ->
    case systest_config:get_env(Attr) of
        {Attr, _Value}=Env -> setelement(3, Acc, [Env|Output]);
        _                  -> Acc
    end;
process({environment, Attr, Value}, {_, _, Output, _, _}=Acc) ->
    setelement(3, Acc, [{Attr, Value}|Output]);
process(Data, {_, _, _, Output, _}=Acc) ->
    setelement(4, Acc, Output ++ [Data]).

