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

%% TODO: migrate to ?SYSTEST_LOG

%% API Exports

-export([init/1, handle_stop/2, handle_kill/2]).
-export([handle_status/2, handle_interaction/3,
         handle_msg/3, terminate/3]).

%% private record for tracking...
-record(sh, {command, args, env, node, state, log, rpc_enabled,
             pid, port, shutdown, detached, shutdown_port}).

-include("systest.hrl").

-ifdef(TEST).
%% TODO: deprecate this in favour of systest_config:eval/2
-export([convert_flags/4]).
-endif.

%%
%% systest_node API
%%

init(Node=#'systest.node_info'{config=Config}) ->
    %% TODO: provide a 'get_multi' version that avoids traversing repeatedly
    Cmd = systest_config:eval("flags.start.program", Config,
                    [{callback, {node, fun systest_node:get_node_info/2}},
                     {return, value}]),
    Env = systest_config:eval("flags.start.environment", Config,
                    [{callback, {node, fun systest_node:get_node_info/2}},
                     {return, value}]),
    Args = case ?ENCONFIG("flags.start.args", Config) of
               not_found -> [];
               Argv      -> Argv
           end,
    Extra = [{env, Env}|?CONFIG(on_start, Config, [])],

    Scope = systest_node:get_node_info(scope, Node),
    Id = systest_node:get_node_info(id, Node),
    Config = systest_node:get_node_info(config, Node),
    Flags = systest_node:get_node_info(flags, Node),

    Startup = ?CONFIG(startup, Config, []),
    Detached = ?REQUIRE(detached, Startup),
    {RpcEnabled, ShutdownSpec} = ?CONFIG(rpc_enabled,
                                         Startup, {true, default}),

    case check_command(Cmd, Detached, RpcEnabled) of
        ok ->
            Env = case ?CONFIG(env, Extra, undefined) of
                      not_found -> [];
                      []        -> [];
                      Other     -> [{env, Other}]
                  end,
            ExecutableCommand = maybe_patch_command(Cmd, Env, Args,
                                                    Detached, RpcEnabled),

            LaunchOpts = [exit_status, hide, stderr_to_stdout,
                          use_stdio, {line, 16384}] ++ Env,

            Shutdown = stop_flags(Flags, ShutdownSpec, RpcEnabled),
            Port = open_port(ExecutableCommand, Detached, Args, LaunchOpts),
            if Detached =:= true -> link(Port);
                            true -> ok
            end,

            on_startup(Scope, Id, Port, Detached, RpcEnabled, Env, Config,
                fun(Port2, Pid, LogFd) ->
                    %% NB: as not all kinds of nodes can be contacted
                    %% via rpc, we have to do this manually here....
                    if RpcEnabled =:= true -> erlang:monitor_node({Id, true});
                                      true -> ok
                    end,

                    N2 = Node#'systest.node_info'{os_pid=Pid},
                    Sh = #sh{pid=Pid,
                             port=Port2,
                             detached=Detached,
                             log=LogFd,
                             rpc_enabled=RpcEnabled,
                             shutdown=Shutdown,
                             command=ExecutableCommand,
                             args=Args,
                             env=Env,
                             state=running},
                    ct:pal(info,
                           "External Process Handler ~p::~p"
                           " Started at ~p~n", [Scope, Id, self()]),
                    {ok, N2, Sh}
                end);
        StopError ->
            StopError
    end.



%% @doc handles interactions with the node.
%% handle_interaction(Data, Node, State) -> {reply, Reply, NewNode, NewState} |
%%                                          {reply, Reply, NewState} |
%%                                          {stop, Reason, NewNode, NewState} |
%%                                          {stop, Reason, NewState} |
%%                                          {NewNode, NewState} |
%%                                          NewState.
%%
%% NB: interactions via rpc (for suitably enabled nodes)
%% is handled generically by systest_node, so callback modules
%% need only deal with more specific scenarios
handle_interaction(_Data, _Node, Sh=#sh{port=detached}) ->
    {{error, detached}, Sh};
handle_interaction(Data, _Node, Sh=#sh{port=Port}) ->
    port_command(Port, Data, [nosuspend]),
    {ok, Sh}.

%% @doc handles a status request from the server.
%% handle_status(Node, State) -> {reply, Reply, NewNode, NewState} |
%%                               {reply, Reply, NewState} |
%%                               {stop, NewNode, NewState}.
handle_status(Node, Sh=#sh{rpc_enabled=true}) ->
    {reply, systest_node:status_check(Node#'systest.node_info'.id), Sh};
handle_status(_Node, Sh=#sh{rpc_enabled=false, state=ProgramState}) ->
    %% TODO: this is wrong - we should spawn and use gen_server:reply
    %%       especially in light of the potential delay in running ./stop
    case ProgramState of
        running -> {reply, nodeup, Sh};
        stopped -> {reply, {nodedown, stopped}, Sh};
        Other   -> {reply, Other, Sh}
    end.

%% @doc handles a kill instruction from the server.
%% handle_kill(Node, State) -> {NewNode, NewState} |
%%                             {stop, NewNode, NewState} |
%%                             NewState.
handle_kill(#'systest.node_info'{os_pid=OsPid},
            Sh=#sh{detached=true, state=running}) ->
    systest:sigkill(OsPid),
    Sh#sh{state=killed};
handle_kill(_Node, Sh=#sh{port=Port, detached=false, state=running}) ->
    ct:pal("kill instruction received - terminating port ~p~n", [Port]),
    Port ! {self(), close},
    Sh#sh{state=killed}.

%% @doc handles a stop instruction from the server.
%% handle_stop(Node, State) -> {NewNode, NewNode} |
%%                             {stop, NewNode, NewState} |
%%                             {rpc_stop, {M,F,A}, NewState} |
%%                             NewState.
handle_stop(Node, Sh=#sh{shutdown=script_stop}) ->
    ct:pal("running shutdown hooks for ~p",
           [systest_node:get_node_info(id, Node)]),
    Flags = systest_node:get_node_info(flags, Node),
    Config = systest_node:get_node_info(config, Node),
    {Env, Args, Prog} = convert_flags(stop, Node, Flags, Config),
    run_shutdown_hook(Sh, Prog, Args, Env);
%% TODO: could this be core node behaviour?
handle_stop(_Node, Sh=#sh{shutdown=Shutdown, rpc_enabled=true}) ->
    %% TODO: this rpc/call logic should move into systest_node
    Halt = case Shutdown of
               default -> {init, stop, []};
               Custom  -> Custom
           end,
    % apply(rpc, call, [Node#'systest.node_info'.id|tuple_to_list(Halt)]),
    {rpc_stop, Halt, Sh#sh{state=stopped}}.
%% TODO: when rpc_enabled=false and shutdown is undefined???

%% @doc handles generic messages from the server.
%% handle_msg(Msg, Node, State) -> {reply, Reply, NewNode, NewState} |
%%                                 {reply, Reply, NewState} |
%%                                 {stop, Reason, NewNode, NewState} |
%%                                 {stop, Reason, NewState} |
%%                                 {NewNode, NewState} |
%%                                 NewState.
handle_msg(sigkill, #'systest.node_info'{os_pid=OsPid},
           Sh=#sh{state=running}) ->
    systest:sigkill(OsPid),
    Sh#sh{state=killed};
handle_msg({Port, {data, {_, Line}}}, _Node,
            Sh=#sh{port=Port, log=LogFd}) ->
    io:format(LogFd, "~s~n", [Line]),
    Sh;
handle_msg({Port, {exit_status, 0}}, _Node,
            Sh=#sh{port=Port, command=Cmd}) ->
    ct:pal("Program ~s exited normally (status 0)~n", [Cmd]),
    {stop, normal, Sh#sh{state=stopped}};
handle_msg({Port, {exit_status, Exit}=Rc}, Node,
             Sh=#sh{port=Port, state=State}) ->
    ct:pal("Node ~p shut down with error/status code ~p~n",
                      [Node#'systest.node_info'.id, Exit]),
    ShutdownType = case State of
                       killed -> normal;
                       _      -> Rc
                   end,
    {stop, ShutdownType, Sh#sh{state=stopped}};
handle_msg({Port, closed}, Node, Sh=#sh{port=Port,
                                        state=killed,
                                        detached=false}) ->
    ct:pal("~p (attached) closed~n", [Port]),
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
    {stop, normal, Sh};
handle_msg({Port, closed}, _Node, Sh=#sh{port=Port}) ->
    ct:pal("~p closed~n", [Port]),
    {stop, {port_closed, Port}, Sh};
handle_msg({'EXIT', Pid, ok}, _Node, Sh=#sh{shutdown_port=Pid,
                                            detached=Detached,
                                            state=killed}) ->
    ct:pal("Termination Port completed ok~n"),
    case Detached of
        true  -> {stop, normal, Sh};
        false -> Sh
    end;
handle_msg({'EXIT', Pid, {error, Rc}}, _Node, Sh=#sh{shutdown_port=Pid}) ->
    ct:pal("Termination Port stopped abnormally (status ~p)~n", [Rc]),
    {stop, termination_port_error, Sh};
handle_msg(Info, _Node, Sh=#sh{state=St, port=P, shutdown_port=SP}) ->
    ct:log("Ignoring Info Message:  ~p~n"
           "State:                  ~p~n"
           "Port:                   ~p~n"
           "Termination Port:       ~p~n",
           [Info, St, P, SP]),
    Sh.

%% @doc gives the handler a chance to clean up prior to being fully stopped.
terminate(Reason, _Node, #sh{port=Port, log=Fd}) ->
    ct:pal("Terminating due to ~p~n", [Reason]),
    %% TODO: verify that we're not *leaking* ports if we fail to close them
    case Fd of
        user -> ok;
        _    -> catch(file:close(Fd))
    end,
    case Port of
        detached -> ok;
        _Port    -> catch(port_close(Port)),
                    ok
    end.

%%
%% Private API
%%

on_startup(Scope, Id, Port, Detached, RpcEnabled, Env, Config, StartFun) ->
    %% we do the initial receive stuff up-front
    %% just to avoid any initial ordering problems...

    Startup = ?CONFIG(startup, Config, []),
    LogEnabled = ?CONFIG(log_enabled, Startup, true),
    {LogName, LogFd} = case LogEnabled of
                           true ->
                               LogFile = log_file("-stdio.log", Scope,
                                                  Id, Env, Config),
                               {ok, Fd2} = file:open(LogFile, [write]),
                               {LogFile, Fd2};
                           false ->
                               {"console", user}
                       end,

    ct:pal("Reading OS process id for ~p from ~p~n"
           "RPC Enabled: ~p~n"
           "StdIO Log: ~s~n",
           [Id, Port, RpcEnabled, LogName]),
    case read_pid(Id, Port, Detached, RpcEnabled, LogFd) of
        {error, {stopped, Rc}} ->
            {stop, {launch_failure, Rc}};
        {error, Reason} ->
            {stop, {launch_failure, Reason}};
        {Port2, Pid, LogFd} ->
            StartFun(Port2, Pid, LogFd)
    end.

log_file(Suffix, Scope, Id, Env, Config) ->
    log_to(Suffix, Scope, Id,
           ?CONFIG(log_dir, Env, default_log_dir(Config))).

log_to(Suffix, Scope, Id, Dir) ->
    filename:join(Dir, logfile(Scope, Id) ++ Suffix).

open_port(ExecutableCommand, Detached, Args, LaunchOpts) ->
    ct:pal("Spawning executable~n"
           "Command:         ~s~n"
           "Detached:        ~p~n"
           "Args:            ~p~n"
           "Launch:          ~p~n",
           [ExecutableCommand, Detached, Args, LaunchOpts]),
    case Detached of
        false -> open_port({spawn_executable, ExecutableCommand},
                           [{args, Args}|LaunchOpts]);
        true  -> open_port({spawn, ExecutableCommand}, LaunchOpts)
    end.

stop_flags(Flags, ShutdownSpec, RpcEnabled) ->
    case ?CONFIG(stop, Flags, undefined) of
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

read_pid(NodeId, Port, Detached, RpcEnabled, Fd) ->
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
                                                  Detached, RpcEnabled, Fd);
                                false -> {error, no_pid}
                            end;
                        {Port, {exit_status, Rc}} ->
                            {error, {stopped, Rc}};
                        {Port, {data, {_, Line}}} ->
                            io:format(Fd, "[~p] ~s~n", [NodeId, Line]),
                            %% NB: the 'launch' process has sent us a pid, but
                            %% that's meaningless for detached nodes until we
                            %% can successfully rpc to get the actual pid.
                            case Detached of
                                true  -> wait_for_nodeup(NodeId);
                                false -> ok
                            end,
                            read_pid(NodeId, Port, Detached, RpcEnabled, Fd);
                        Other ->
                            io:format(Fd, "[~p] ~p~n", [NodeId, Other]),
                            read_pid(NodeId, Port, Detached, RpcEnabled, Fd)
                    after 5000 ->
                        ct:log("timeout waiting for os pid... re-trying~n"),
                        read_pid(NodeId, Port, Detached, RpcEnabled, Fd)
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
process({environment, Key, {node, Attr}}, {Node, _, Output, _, _}=Acc) ->
    Value = convert_node_attribute(Attr, Node),
    setelement(3, Acc, [{Key, Value}|Output]);
process({environment, Attr, Value}, {_, _, Output, _, _}=Acc) ->
    setelement(3, Acc, [{Attr, Value}|Output]);
process(Data, {_, _, _, Output, _}=Acc) ->
    setelement(4, Acc, Output ++ [Data]).

convert_node_attribute(Attr, Node) ->
    case lists:member(Attr, [name, host, id]) of
        true  -> atom_to_list(systest_node:get_node_info(Attr, Node));
        false -> systest_node:get_node_info(Attr, Node)
    end.

default_log_dir(Config) ->
    ?CONFIG(scratch_dir, Config, systest_utils:temp_dir()).

logfile(Scope, Id) ->
    atom_to_list(Scope) ++ "-" ++ atom_to_list(Id).

