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
             rpc_enabled, pid, port, shutdown, detached,
             shutdown_port}).

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
    case is_process_alive(Server) of
        false -> ct:pal("Server process down!~n"), {'nodedown', unknown};
        true  -> gen_server:call(Server, ping)
    end.

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
    process_flag(trap_exit, true),

    Id = systest_node:get_node_info(id, Node),
    Config = systest_node:get_node_info(config, Node),
    Flags = systest_node:get_node_info(flags, Node),
    Startup = ?CONFIG(startup, Config, []),
    Detached = ?REQUIRE(detached, Startup),
    LogEnabled = ?CONFIG(log_enabled, Startup, true),
    {RpcEnabled, ShutdownSpec} = ?CONFIG(rpc_enabled,
                                         Startup, {true, default}),

    case check_command(Cmd, Detached, RpcEnabled) of
        ok ->
            Env = case ?CONFIG(env, Extra, undefined) of
                      undefined -> [];
                      []        -> [];
                      Other     -> [{env, Other}]
                  end,
            ExecutableCommand = maybe_patch_command(Cmd, Env, Args,
                                                    Detached, RpcEnabled),

            LaunchOpts = [exit_status, hide, stderr_to_stdout,
                          use_stdio, {line, 16384}] ++ Env,

            Shutdown = case ?CONFIG(stop, Flags, undefined) of
                           undefined ->
                               case RpcEnabled of
                                   %% eh!? this needs to be a {stop, ReturnVal}
                                   false -> throw(shutdown_spec_missing);
                                   true  -> ShutdownSpec
                               end;
                           {call, M, F, Argv} ->
                               {M, F, Argv};
                           Spec when is_list(Spec) ->
                               script_stop
                       end,

            Port = case Detached of
                       false -> ct:pal("Spawning executable~n"
                                       "Command: ~s~n"
                                       "Args: ~p~n", [ExecutableCommand,Args]),
                                P = open_port(
                                        {spawn_executable, ExecutableCommand},
                                        [{args, Args}|LaunchOpts]),
                                true = link(P), P;
                       true  -> ct:pal("Spawning command~n"
                                       "Command: ~s~n", [ExecutableCommand]),
                                open_port({spawn, ExecutableCommand},
                                          LaunchOpts)
                   end,

            %% we do the initial receive stuff up-front
            %% just to avoid any initial ordering problems...
            ct:pal("Reading OS process id for ~p from ~p~n",
                   [Id, Port]),
            case read_pid(Id, Port, Detached, RpcEnabled) of
                {error, {stopped, Rc}} ->
                    {stop, {launch_failure, Rc}};
                {error, Reason} ->
                    {stop, {launch_failure, Reason}};
                {Port2, Pid} ->
                    net_kernel:monitor_nodes(true),
                    Sh = #sh{pid=Pid,
                             port=Port2,
                             detached=Detached,
                             log_enabled=LogEnabled,
                             rpc_enabled=RpcEnabled,
                             shutdown=Shutdown,
                             command=ExecutableCommand,
                             state=running,
                             node=Node#'systest.node_info'{os_pid=Pid}},
                    ct:pal(info,
                           "External Process Handler Started at ~p~n",
                           [self()]),
                    {ok, Sh}
            end;
        StopError ->
            StopError
    end.

handle_call(os_pid, _From, Sh=#sh{pid={detached, Pid}}) ->
    {reply, Pid, Sh};
handle_call(os_pid, _From, Sh=#sh{pid=Pid}) ->
    {reply, Pid, Sh};
handle_call({command, Data}, _From, Sh=#sh{port=detached}) ->
    {stop, {error, detached}, Sh};
handle_call({command, Data}, _From, Sh=#sh{port=Port}) ->
    port_command(Port, Data, [nosuspend]),
    {reply, ok, Sh};
handle_call(ping, _From, Sh=#sh{rpc_enabled=true, node=Node}) ->
    {reply, systest_node:status_check(Node#'systest.node_info'.id), Sh};
handle_call(ping, _From, Sh=#sh{rpc_enabled=false, state=ProgramState}) ->
    %% TODO: this is wrong - we should spawn and use gen_server:reply
    %%       especially in light of the potential delay in running ./stop
    case ProgramState of
        running       -> {reply, nodeup, Sh};
        stopped       -> {reply, {nodedown, stopped}, Sh};
        wait_shutdown -> {reply, wait_shutdown, Sh}
    end;
handle_call(_Msg, _From, Sh) ->
    {noreply, Sh}.

handle_cast(kill, Sh=#sh{state=stopped}) ->
    {noreply, Sh};
handle_cast(kill, Sh=#sh{node=Node, detached=true, state=running}) ->
    systest_node:sigkill(Node),
    {noreply, Sh#sh{state=killed}};
handle_cast(kill, Sh=#sh{port=Port, detached=false, state=running}) ->
    ct:pal("kill instruction received - terminating port ~p~n", [Port]),
    Port ! {self(), close},
    {noreply, Sh#sh{state=killed}};

handle_cast(stop, Sh=#sh{shutdown=stopped}) ->
    {noreply, Sh};
handle_cast(stop, Sh=#sh{node=Node, shutdown=script_stop}) ->
    ct:pal("running shutdown hooks for ~p",
           [systest_node:get_node_info(id, Node)]),
    Flags = systest_node:get_node_info(flags, Node),
    Config = systest_node:get_node_info(config, Node),
    {Env, Args, Prog} = convert_flags(stop, Node, Flags, Config),
    {noreply, run_shutdown_hook(Sh, Prog, Args, Env)};
handle_cast(stop, Sh=#sh{node=Node, shutdown=Shutdown, rpc_enabled=true}) ->
    Halt = case Shutdown of
               default -> {init, stop, []};
               Custom  -> Custom
           end,
    apply(rpc, call, [Node#'systest.node_info'.id|tuple_to_list(Halt)]),
    {noreply, Sh#sh{state=stopped}};
%% TODO: when rpc_enabled=false and shutdown is undefined???
handle_cast(_Msg, Sh) ->
    {noreply, Sh}.

handle_info({nodedown, NodeId},
            Sh=#sh{state=State, node=#'systest.node_info'{id=NodeId}}) ->
    ShutdownType = case State of
                       killed  -> normal;
                       stopped -> normal;
                       _       -> nodedown
                   end,
    {stop, ShutdownType, Sh};
handle_info({nodedown, NodeId},
            Sh=#sh{state=_, node=#'systest.node_info'{id=NodeId}}) ->
    {stop, nodedown, Sh};
handle_info({Port, {data, {_, Line}}},
            Sh=#sh{port=Port, node=#'systest.node_info'{id=Id}}) ->
    ct:log("[~p] " ++ Line, [Id]),
    {noreply, Sh};
handle_info({Port, {exit_status, 0}},
            Sh=#sh{port=Port, log_enabled=Pal, command=Cmd}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["Program ~s exited normally (status 0)~n", [Cmd]]),
    {stop, normal, Sh#sh{state=stopped}};
handle_info({Port, {exit_status, Exit}=Rc},
             Sh=#sh{port=Port, state=State, log_enabled=Pal, node=Node}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["Node ~p shut down with error/status code ~p~n",
                      [Node#'systest.node_info'.id, Exit]]),
    ShutdownType = case State of
                       killed -> normal;
                       _      -> Rc
                   end,
    {stop, ShutdownType, Sh#sh{state=stopped}};
handle_info({Port, closed}, Sh=#sh{port=Port, log_enabled=Pal, node=Node,
                                   state=killed, detached=false}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["~p (attached) closed~n", [Port]]),
    case Sh#sh.rpc_enabled of
        true ->
            %% to account for a potential timing issue when the calling test
            %% execution process is sitting in `kill_and_wait` - we force a
            %% call to net_adm:ping/1, which gives the net_kernel time to get
            %% its knickers in order before proceeding....
            Id = systest_node:get_node_info(id, Node),
            systest_node:status_check(Id);
        false ->
            ok
    end,
    %% ct:pal("Node Status: ~p~n", [systest_node:status_check(Id)]),
    {stop, normal, Sh};
handle_info({Port, closed}, Sh=#sh{port=Port, log_enabled=Pal}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["~p closed~n", [Port]]),
    {stop, {port_closed, Port}, Sh};
handle_info({'EXIT', Pid, ok}, Sh=#sh{shutdown_port=Pid, state=killed,
                               detached=Detached, log_enabled=Pal}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["Termination Port completed ok~n"]),
    case Detached of
        true  -> {stop, normal, Sh};
        false -> {noreply, Sh}
    end;
handle_info({'EXIT', Pid, {error, Rc}},
            Sh=#sh{shutdown_port=Pid, log_enabled=Pal}) ->
    LogFun = case Pal of true -> pal; _ -> log end,
    apply(ct, LogFun, ["Termination Port stopped abnormally (status ~p)~n",
                      [Rc]]),
    {stop, termination_port_error, Sh};
handle_info(Info, Sh) ->
    ct:pal("Ignoring ~p~n", [Info]),
    {noreply, Sh}.

terminate({port_closed, _}, _) ->
    ok;
terminate(Reason, #sh{port=Port}) ->
    ct:pal("Terminating due to ~p~n", [Reason]),
    %% TODO: verify that we're not *leaking* ports if we fail to close them
    case Port of
        detached -> ok;
        _Port    -> catch(port_close(Port)),
                    ok
    end.

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

run_shutdown_hook(Sh, Prog, Args, Env) ->
    ct:pal("Spawning executable~n"
           "Command: ~s~nArgs: ~p~n", [Prog, Args]),
    Pid= spawn_link(fun() ->
                        Port = open_port({spawn_executable, Prog},
                                         [{env, Env}, {line, 16384},
                                         use_stdio, stderr_to_stdout,
                                         exit_status, {args, Args}]),
                        exit(shutdown_loop(Port))
                    end),
    Sh#sh{shutdown_port=Pid, state=stopped}.

%% port handling

shutdown_loop(Port) ->
    receive
        {Port, {exit_status, 0}}  -> ok;
        {Port, {exit_status, Rc}} -> {error, Rc};
        {Port, {data, {_, _}}}    -> shutdown_loop(Port)
    end.

read_pid(NodeId, Port, Detached, RpcEnabled) ->
    case RpcEnabled of
        true ->
            case rpc:call(NodeId, os, getpid, []) of
                {badrpc, _Reason} ->
                    receive
                        {Port, {exit_status, 0}} ->
                            case Detached of
                                %% NB: with detached nodes, the 'launcher' will
                                %% exit leaving the node up and running, so we
                                %% now need to sit in a loop until we can rpc
                                true  -> read_pid(NodeId, Port,
                                                  Detached, RpcEnabled);
                                false -> {error, no_pid}
                            end;
                        {Port, {exit_status, Rc}} ->
                            {error, {stopped, Rc}};
                        {Port, {data, {eol, _Pid}}} ->
                            %% NB: the 'launch' process has sent us a pid, but
                            %% that's meaningless for detached nodes until we
                            %% can successfully rpc to get the actual pid.
                            case Detached of
                                true  -> wait_for_nodeup(NodeId);
                                false -> ok
                            end,
                            read_pid(NodeId, Port, Detached, RpcEnabled);
                        Other ->
                            ct:log("[~p] ~p", [NodeId, Other]),
                            read_pid(NodeId, Port, Detached, RpcEnabled)
                    after 5000 ->
                        ct:log("timeout waiting for os pid... re-trying~n"),
                        read_pid(NodeId, Port, Detached, RpcEnabled)
                    end;
                Pid ->
                    case Detached of
                        false -> {Port, Pid};
                        true  -> {detached, Pid}
                    end
            end;
        false ->
            %% NB: detached + rpc_disabled is currently disallowed, so we don't
            %% cater for {detached, Pid} here at all.
            receive
                {Port, {data, {eol, Pid}}} -> Pid;
                {Port, {exit_status, Rc}}  -> {error, {stopped, Rc}}
            end
    end.

wait_for_nodeup(NodeId) ->
    case net_kernel:connect(NodeId) of
        true    -> ok;
        _       -> erlang:yield(), wait_for_nodeup(NodeId)
    end.

%% command processing

check_command(_, false, true) ->
    ok;
check_command(_, true, false) ->
    %% TODO: think about if/how we can relax this rule....
    {stop, {error, {detached, no_rpc}}};
check_command(Cmd, true, true) ->
    case re:run(Cmd, "(&&|;)") of
        nomatch -> ok;
        _       -> {stop, async_multicmds_disallowed}
    end.

maybe_patch_command(Cmd, _, _, false, true) ->
    Cmd;
maybe_patch_command(Cmd, Env, Args, Detached, RpcEnabled) when Detached orelse
                                                               RpcEnabled ->
    case os:type() of
        {win32, _} ->
            %% TODO: the argv conversion thing here....
            "cmd /q /c " ++ lists:foldl(fun({Key, Value}, Acc) ->
                                        expand_env_variable(Acc, Key, Value)
                                        end, Cmd, Env);
        _ ->
            Exec = string:join([Cmd|Args], " "),
            "/usr/bin/env sh -c \"echo $$; exec " ++ Exec ++ "\""
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
    ct:pal("flags: ~p~n", [Flags]),
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

