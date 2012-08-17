%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
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
%% @doc
%% Provides configurable tracing on managed/remote nodes. Trace configuration
%% can be supplied on the command line, or stored in configuration files which
%% are enabled on the comand line instead.
%%
%% Some examples of command line usage follow.
%%
%% 1. trace config + bindings stored in configuration files
%% ./systest -t ./trace.config -T file:trace-bindings.config
%%
%% 2. load trace config and enable a named trace setup at the given scope
%% ./systest -t ./resources/trace.config -T my_test_SUITE+trace_all
%%
%% 3. use the default trace config and enable stuff for a given scope
%% ./systest -T my_test_SUITE+gc -T my_test_SUITE+pcall
%%
%% 4. use the default trace config and customise built-in pcall
%% ./systest -T my_test_SUITE:+pcall                        \
%%              --trace-pcall-location=n1@iske,n2@frigg     \
%%              --trace-pcall-pids=management_db            \
%%              --trace-pcall-pids=new                      \
%%              --trace-pcall-mod=management_db             \
%%              --trace-pcall-func=execute_query            \
%%              --trace-pcall-mfa=management:do_it/1
%%
%% 5. enable tracing to the console as well as the log files
%% ./systest -C -T my_test_function_name:gc
%%
%% @end
%% ----------------------------------------------------------------------------
-module(systest_trace).

-include("systest.hrl").

-export([debug/2, stop/1, print_trace_info/1]).
-export([log_trace_file/1, write_trace_file/2]).
-compile(export_all).
-define(TRACE_DISABLED, {trace, disabled}).

-import(systest_utils, [as_list/1]).

-include("systest_trace.hrl").

-exprecs_prefix([operation]).
-exprecs_fname(["record_", prefix]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records([trace]).

log(F, A) -> io:format(user, F, A).

%convert_history_entry({ttb, tracer, [Nodes, []]}, Acc) ->
%    Acc#trace_config{locations=Nodes};
%convert_history_entry({ttb, p, [Loc, [timestamp|Spec]]}, Acc) ->
%    Type = case Spec of
%               garbage_collection -> gc;
%               running            -> sched;
%
%           end.

load(Config) ->
    %{trace, {local, File}}
    BaseDir = ?REQUIRE(base_dir, Config),
    ScratchDir = ?REQUIRE(scratch_dir, Config),
    TraceData = filename:join([ScratchDir, "trace"]),
    TraceDbDir = filename:join([ScratchDir, "db", "trace"]),
    TraceConfigFile = ?CONFIG(trace_config, Config,
                              default_config_file(BaseDir)),
    Console = ?CONFIG(trace_console, Config, false),
    Flush = if Console == false -> ?CONFIG(trace_flush, Config, false);
                           true -> false
            end,

    SysConf = #trace_config{trace_config=TraceConfigFile,
                            trace_db_dir=TraceDbDir,
                            trace_data_dir=TraceData,
                            flush=Flush,
                            console=Console},
    find_activated(Config, SysConf).

print_trace_info(#trace_config{active=[]}) ->
    ok;
print_trace_info(SysConf) ->
    Prop = systest_utils:record_to_proplist(SysConf, systest_trace_config),
    {value, {active, Active}, Prop2} = lists:keytake(active, 1, Prop),
    systest_utils:print_section("Trace Configuration",
                                lists:keydelete(base_config, 1, Prop2)),
    Traces = [begin
                 PList = systest_utils:record_to_proplist(T,
                                                        systest_trace),
                 {value, {_, TP}, PList2} =
                        lists:keytake(trace_pattern, 1, PList),
                 TP2 = systest_utils:record_to_proplist(TP,
                                            systest_trace_pattern),
                 PList2 ++ [{trace_pattern, TP2}]
              end || T <- Active],
    [systest_utils:print_section("Active Trace", T) || T <- Traces].

find_activated(Config, SysConf=#trace_config{trace_config=TraceConfigFile}) ->
    BaseTraceConfig = read_config(TraceConfigFile),
    {Enabled, _} = apply_flags(Config, BaseTraceConfig),
    SysConf#trace_config{active=Enabled, base_config=BaseTraceConfig}.

%% @private
%% We must take all enabled traces and look up their configuration from the
%% trace config file (or defaults). Config that is *missing* from these
%% locations MUST be available in the supplied flags/args, otherwise we bail.
%% @end
apply_flags(Config, BaseTraceConfig) ->
    TraceSettings = override_defaults_with_user_args(BaseTraceConfig, Config),
    Enabled = proplists:get_all_values(trace_enable, Config),
    lists:foldl(
        fun(Flag, {Acc, Base}) ->
            case string:tokens(Flag, "+") of
                [_] ->
                    %% A simple enable flag that means 'turn on [trace-name]'.
                    %% In this case, we look for user-defined config associated
                    %% with the name, which allows users to define a trace in
                    %% their config file(s) for a given scope and enable it
                    %% with `-T scope_name` on the command line
                    Trace = build_trace(list_to_atom(Flag), Base),
                    {[Trace#trace{scope=all}|Acc], Base};
                [Scope, Target] ->
                    %% -T scope+target trace flags indicate that for 'Scope'
                    %% we should enable the trace named 'Target', so....
                    ScopeName = list_to_atom(Scope),
                    InitTrace = build_trace(list_to_atom(Target), Base),
                    {[InitTrace#trace{scope=ScopeName}|Acc], Base}
            end
        end, {[], TraceSettings}, Enabled).

build_trace(TraceName, Base) ->
    {TraceConfig, _RemainingBase} =
                read_trace_config(TraceName, Base),
    TP = systest_trace_pattern:record_fromlist(
                ?REQUIRE(trace_pattern, TraceConfig)),
    Location = ?CONFIG(location, TraceConfig, 'all'),
    #trace{name=TraceName,
           scope=TraceName,
           process_filter=?REQUIRE(process_filter, TraceConfig),
           trace_flags=?REQUIRE(trace_flags, TraceConfig),
           trace_pattern=TP,
           location=Location}.

%% @private
%% flags passed on the command line have a similar structure, but
%% are prefixed and need reprocessing and merging with whatever
%% configuration we've been able to load
%% @end
maybe_merge_config(_TraceConfig, _Flag, _Config) ->
    %% TraceConfig could be 'noconfig' or proplist() where
    %% --trace-pcall-location has been transformed into a structure
    %% such as `{pcall, [{location, Value}]}`
    ok.

%% @private
%% take the supplied UserConfig and for each trace key (passed on the command
%% line as --trace-[name]-[setting]=[value] and supplanted during argument
%% parsing with {'trace-name-setting', value} tuples); we will either add it
%% to the BaseConfig (loaded from a user defined file or the defaults) or
%% replace any 'settings' for trace 'name' with those passed on the commandline
%% @end
override_defaults_with_user_args(BaseConfig, []) ->
    override_defaults_with_user_args(BaseConfig, BaseConfig);
override_defaults_with_user_args(BaseConfig, UserConfig) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            case lists:prefix("trace", atom_to_list(K)) of
                true ->
                    case string:tokens(atom_to_list(K), "-") of
                        ["trace", Name, Setting]=_Parts ->
                            TraceKey = list_to_atom(Name),
                            NewVal = user_override(list_to_atom(Setting), V),
                            case lists:keyfind(TraceKey, 1, Acc) of
                                false ->
                                    lists:keystore(TraceKey, 1, Acc, NewVal);
                                {TraceKey, Values} ->
                                    Update = {TraceKey, [NewVal|Values]},
                                    lists:keyreplace(TraceKey, 1, Acc, Update)
                            end;
                        _ ->
                            Acc
                    end;
                false ->
                    Acc
            end
        end, BaseConfig, UserConfig).

user_override(location, Locations) ->
    NodeNames = [list_to_atom(Node) || Node <- string:tokens(Locations, ",")],
    {location, NodeNames};
user_override(pids, Pids) ->
    {process_filter, [list_to_atom(Pid) || Pid <- string:tokens(Pids, ",")]};
user_override(TraceKey, Value) ->
    {TraceKey, Value}.

strip_trace_prefix(FlagName) ->
    lists:sublist(FlagName, 9, length(FlagName) - 7).

%% @private read the trace config for name, de-referencing aliased config
%% if necessary - we remove top-level elements, but referenced/aliased are
%% shared so we don't remove them.
%% @end
read_trace_config(TraceName, Config) ->
    case lists:keytake(TraceName, 1, Config) of
        false ->
            %% scope enabled but no config!
            {noconfig, Config};
        {value, {_, CfgRef}, Rest}
                    when is_atom(CfgRef) ->
            case lists:keyfind(TraceName, 1, Rest) of
                false ->
                    %% referenced (named) trace config not found!
                    {noconfig, Rest};
                {_, RefCfg} ->
                    {RefCfg, Rest}
            end;
        {value, {_, Cfg}, Rest} ->
            %% we have to deal with trace_defaults ++ overrides
            %% TODO: this pattern exists in systest_resources also -
            %% we should write one set of routines to handle it...
            Processed = 
                lists:foldl(fun(C, Acc) ->
                                Conf = if is_atom(C) -> ?REQUIRE(C, Config);
                                                true -> C
                                       end,
                                case is_list(Conf) of
                                    false -> [Conf|Acc];
                                    true  -> systest_config:merge(
                                                as_list(Conf), Acc)
                                end
                            end, [], Cfg),
            {Processed, Rest}
    end.

%read_config("Z") ->
%    application:get_env(systest, default_config_file);
read_config(Path) ->
    case filelib:is_regular(Path) of
        false -> default_config();
        true  -> {ok, Data} = file:consult(Path),
                 systest_config:merge(default_config(), Data)
    end.

default_config() ->
    {ok, Config} = application:get_env(systest, default_trace_config),
    Config.

default_config_file(BaseDir) ->
    filename:join([BaseDir, "resources", "trace.config"]).

%%%%%%%%% OLD API %%%%%%%%%%%

log_trace_file(TraceFile) ->
    write_trace_file(TraceFile, user).

write_trace_file(TraceFile, [H|_]=TargetFile) when is_integer(H) ->
    {ok, Fd} = file:open(TargetFile, [write, binary]),
    try
        write_trace_file(TraceFile, Fd)
    after
        file:close(Fd)
    end;
write_trace_file(TraceFile, TargetFd) ->
    %% TODO: this should be wired into our logging subsystem (when it appears!)
    CPid = dbg:trace_client(file, TraceFile,
                            {fun trace_writer/2, TargetFd}),
    dbg:stop_trace_client(CPid).

%% a *very* rudimentary console trace writer
trace_writer({trace,_,call,{M,F,A}, PState}, Fd) ->
    io:format(Fd, "[CALL] ~p:~p(~p)~n", [M, F, A]),
    if is_binary(PState) =:= true ->
        io:format(Fd, "[PSTATE] ~s~n", [binary_to_list(PState)]);
       true ->
        ok
    end,
    io:format(Fd, "~n", []),
    Fd;
trace_writer({trace,_,return_from,{M,F,A},R}, Fd) ->
    io:format(Fd, "[RETURN] ~p:~p/~p => ~p~n",
              [M, F, A, R]), Fd;
trace_writer(Trace, Fd) ->
    io:format(Fd, "[TRACE] ~p~n", [Trace]), Fd.

debug(TestCase, Config) ->
    case load_trace_configuration({ct, TestCase}, Config) of
        {TraceName, {enabled, TraceTargets}} ->
            systest_log:log(framework,
                "tracing enabled for ~p: ~p~n",
                [TestCase, TraceTargets]),
            update_config(Config, TraceName, TraceTargets);
        ?TRACE_DISABLED ->
            systest_log:log(framework,
                "tracing disabled for ~p~n", [TestCase]),
            Config
    end.

stop(Config) ->
    %% should this not be calling dbg:stop() as well!?
    case lists:keysearch(traces, 1, Config) of
        {value, {traces, TraceList}} when is_list(TraceList) ->
            unload_all_trace_configuration(TraceList);
        _ -> ok
    end.

load_trace_configuration({ct, TestCase}, _Config) ->
    case ct:get_config({debug,test_cases}) of
        undefined ->
            ?TRACE_DISABLED;
        TraceConfig ->
            load_tc_trace_config(lists:keysearch(TestCase, 1, TraceConfig))
    end.

%%load_trace_configuration(TestCaseOrConfig) when is_atom(TestCaseOrConfig) ->
%% load_tc_trace_config(lists:keysearch(TestCase, 1,
%% ct:get_config({trace_configuration,test_cases}))).

unload_all_trace_configuration([{trace, TraceName,
                                 {Mod, [Func|Funcs], Arity}} | Rest]) ->
    unload_all_trace_configuration([{trace, TraceName,
                                   {Mod, Func, Arity}} | Rest]),
    unload_all_trace_configuration([{trace, TraceName,
                                   {Mod, Funcs, Arity}} | Rest]);
unload_all_trace_configuration([{trace, _TraceName,
                                 {_Mod, [], _Arity}} | _TraceList]) ->
    ok;
unload_all_trace_configuration([{trace, _TraceName,
                                 {Mod, Func, Arity}} | Rest]) ->
    dbg:ctp({Mod, Func, Arity}),
    unload_all_trace_configuration(Rest);
unload_all_trace_configuration([]) ->
    ok.

update_config(Config, TraceName, MFA) ->
    %%TODO: deal with MFA clashes???
    case lists:keysearch(traces, 1, Config) of
        {value, {traces, TraceList}} ->
            lists:keyreplace(traces, 1, Config,
                            {traces, [{trace, TraceName, MFA} | TraceList]});
        false ->
            lists:append(Config, [{traces, [{trace, TraceName, MFA}]}])
    end.

%% TODO: deal with trace specs that come in a list...

load_tc_trace_config({value, {_TestCase, TraceSpec}})
        when is_atom(TraceSpec) ->
  load_actual_trace_config(
    lists:keysearch(TraceSpec, 1,
                    ct:get_config({debug, trace_targets})));
load_tc_trace_config(false) ->
    ?TRACE_DISABLED.

load_actual_trace_config({value,
                        {TraceName, TraceConfig}}) when is_list(TraceConfig) ->
    Mod = proplists:get_value(mod, TraceConfig, '_'),
    Func = proplists:get_value(function, TraceConfig, '_'),
    Arity = proplists:get_value(arity, TraceConfig, '_'),
    MSpec = proplists:get_value(match_spec, TraceConfig,
    %% TODO: default to a simpler setting and in that,
    %% default to return_trace only....
    [{'_',[],[{exception_trace},{return_trace},{message,{process_dump}}]}]),

    %%PFlags = proplists:get_value(flags, TraceConfig, [local]),
    %%TFlags = [call, procs, return_to],
    %%erlang:trace_pattern({Mod, Func, Args}, MSpec, PFlags),
    %%erlang:trace(all, true, TFlags),

    setup_tracer(),
    trace_tpl(Mod, Func, Arity, MSpec),

    %% TODO: bring in the ability to specify the flags
    %% (and to turn this off again!)
    dbg:p(all,[c, return_to]),
    {TraceName, {enabled, {Mod, Func, Arity}}};
load_actual_trace_config(false) ->
    {trace, disabled}.

trace_tpl(Mod, [Func|Funcs], Arity, MSpec) ->
    trace_tpl(Mod, Func, Arity, MSpec),
    trace_tpl(Mod, Funcs, Arity, MSpec);
trace_tpl(_, [], _, _) ->
    ok;
trace_tpl(Mod, Func, Arity, MSpec) ->
    dbg:tpl(Mod, Func, Arity, MSpec).

setup_tracer() ->
    %% TODO: configure this when CT is not in play....
    TracerConfig = ct:get_config({debug, trace_setup},
                                 [{trace_type, process}]),
    TraceType = proplists:get_value(trace_type, TracerConfig),
    setup_tracer(TraceType, TracerConfig).

setup_tracer(process, _) ->
    systest_log:log(framework, "setting up standard tracer on ~p.~n", [self()]),
    dbg:tracer();
setup_tracer(port, TracerConfig) ->
    PortKind = proplists:get_value(port_kind, TracerConfig, ip),
    PortSpec = case PortKind of
                   ip ->
                       %% uhm, that's a stupid default port - change it
                       proplists:get_value(trace_port, TracerConfig, 4711);
                   file ->
                       proplists:get_value(filename, TracerConfig, error)
               end,
    setup_port_tracer(PortKind, PortSpec).

setup_port_tracer(file, error) ->
    ct:fail("Cannot determine default file name for port tracing. Please "
            "set the {filename, FN} tuple in your config file properly.");
setup_port_tracer(PortType, PortSpec) ->
    systest_log:log("configuring ~p tracer on ~p.~n", [PortType, PortSpec]),
    dbg:tracer(port, dbg:trace_port(PortType, PortSpec)).
