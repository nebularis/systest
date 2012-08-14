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
-module(systest_main).

-export([run/1, help/0]).
-import(systest_utils, [record_to_proplist/2]).

help() ->
    io:format("Usage: systest [-P <profile>] [-L <logging>] [-n] [-h]"
              " [?<command>]~n~n"
              "-h               Show the program options~n"
              "-q, --quiet      Disable header/options output on start~n"
              "-P, --profile    Use the specified test profile~n"
              "-Z, --target     Use the specified target~n"
              "-L, --logging    Active logging for the specified sub-system~n"
              "-n, --dryrun     Print everything out but don't run any tests~n"
              "-w, --no_cover   Disable code coverage~n"
              "-a, --name       Specify (Erlang) node name, "
                                "default = systest_runner~n"
              "-A, --longnames  Use long instead of short names with -a~n"
              "-X, --dump       Dump configuration/status information if the "
                                "run fails~n"
              "--<s>-<k>=<v>    Set [s]ubsystem [k]ey to [v]alue~n"
              "commands (optional):~n"
              "inspect-trace [node(s)]~n"
              "~n").

run(["-h"]) ->
    help(),
    erlang:halt(0);
run(["check"|Args]) ->
    systest_utils:print_section("Options", parse_args(Args));
run(["trace"|Args]) ->
    Options = parse_args(Args),
    systest:start(),
    Config = [{base_dir, element(2, file:get_cwd())},
              {scratch_dir, "/tmp/systest"}|Options],
    TraceConfig = systest_trace:load(Config),
    Prop = record_to_proplist(TraceConfig, systest_trace_config),
    {value, {active, Active}, Prop2} = lists:keytake(active, 1, Prop),
    systest_utils:print_section("Trace Configuration", Prop2),
    Traces = [begin
                  PList = record_to_proplist(T, systest_trace),
                  {value, {_, TP}, PList2} =
                        lists:keytake(trace_pattern, 1, PList),
                  TP2 = record_to_proplist(TP, systest_trace_pattern),
                  PList2 ++ [{trace_pattern, TP2}]
              end || T <- Active],
    [systest_utils:print_section("Active Trace", T) || T <- Traces];
run(Args) ->
    Options = parse_args(Args),
    application:load(systest),
    systest_runner:execute(Options).

parse_args(Args) ->
    Mode = case os:type() of
               {win32, _} -> win;
               _          -> unix
           end,
    Spec = opt_spec(),
    OptsWithVals = lists:map(fun erlang:atom_to_list/1,
                            lists:flatten([[L, S] || {L, S, string} <- Spec])),
    {Options, RawOpts} = niceopt:parse(Args, [{mode, Mode},
                                              {opts_with_vals, OptsWithVals}]),
    [{raw_opts, RawOpts}|validate(Options, Spec)].

validate(Options, Spec) ->
    [begin
        case lists:keyfind(K, 1, Spec) of
            false ->
                case lists:keyfind(K, 2, Spec) of
                    false ->
                        RawOpt;
                    Def ->
                        unpack(V, Def)
                end;
            OptDef ->
                unpack(V, OptDef)
        end
     end || {K, V}=RawOpt <- Options].

unpack(true, {L, _, V}) when V =:= integer orelse
                             V =:= string ->
                             io:format("Argument ~p requires a value!~n", [L]),
                             help(),
                             erlang:halt(1);
unpack(V,    {L, _, integer}) -> {L, list_to_integer(V)};
unpack(V,    {target, _, _})  -> case string:tokens(V, ":") of
                                     [Suite] ->
                                         {testsuite, [Suite]};
                                     [Suite, Case] ->
                                         {testcase, {Suite, Case}}
                                 end;
unpack(V,    {L, _, string})  -> {L, V};
unpack(V,    {L, _, flag})    -> {L, V}.

opt_spec() ->
    [{profile,      'P', string},
     {target,       'Z', string},
     {logging,      'L', string},
     {dryrun,       'n', flag},
     {no_cover,     'w', flag},
     {dump,         'X', flag},
     {node,         'a', string},
     {longnames,    'A', flag},
     {trace_config, 't', string},
     {trace_enable, 'T', string},
     {trace_flush,  'F', flag},
     {trace_console,'C', flag},
     {quiet,        'q', flag}].
