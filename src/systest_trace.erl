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
-module(systest_trace).

-include("systest.hrl").

-export([debug/2, stop/1]).
-export([log_trace_file/1, write_trace_file/2]).

-define(TRACE_DISABLED, {trace, disabled}).

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
            ct:pal("tracing enabled for ~p: ~p~n",
                   [TestCase, TraceTargets]),
            update_config(Config, TraceName, TraceTargets);
        ?TRACE_DISABLED ->
            ct:pal("tracing disabled for ~p~n", [TestCase]),
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
    ct:pal(default, "Setting up standard tracer on ~p.~n", [self()]),
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
    ct:pal(io:format("Configuring ~p tracer on ~p.~n", [PortType, PortSpec])),
    dbg:tracer(port, dbg:trace_port(PortType, PortSpec)).
