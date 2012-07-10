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
-module(systest_main2).

-export([run/1, help/0]).

help() ->
    io:format("RTFSC :)~n").

run(Args) ->
    Options = parse_args(Args),
    application:load(systest),
    {module, zip} = code:ensure_loaded(zip),
    File = escript:script_name(),
    case escript:extract(File, []) of
        {ok, [_Shebang, _Comment, _EmuArgs, {archive, ArchiveBin}]} ->
            zip:foldl(fun(Name, _, Bin, _) ->
                          case lists:suffix("banner.txt", Name) of
                              true ->
                                  application:set_env(systest, banner, Bin());
                              false ->
                                  ok
                          end
                      end, ok, {File, ArchiveBin});
        {error, _} = Error ->
            throw(Error)
    end,
    systest_runner:execute(Options).

parse_args(Args) ->
    Mode = case os:type() of
               {win32, _} -> win;
               _          -> unix
           end,
    Spec = opt_spec(),
    OptsWithVals = lists:map(fun erlang:atom_to_list/1,
                             lists:flatten([[L, S] || {L, S, string} <- Spec])),
    io:format(user, "~p :: ~p~n", [Args, OptsWithVals]),
    {Options, _} = niceopt:parse(Args, [{mode, Mode},
                                        {opts_with_vals, OptsWithVals}]),
    lists:foldl(fun(E, Acc) -> systest_config:merge(Acc, [E]) end, [],
                validate(Options, Spec)).

validate(Options, Spec) ->
    [begin
        case lists:keyfind(K, 1, Spec) of
            false ->
                case lists:keyfind(K, 2, Spec) of
                    false ->
                        systest_utils:abort("Invalid Option ~p~n", [K]);
                    Def ->
                        unpack(V, Def)
                end;
            OptDef ->
                unpack(V, OptDef)
        end
     end || {K, V} <- Options].

unpack(V, {L, _, integer}) -> {L, list_to_integer(V)};
unpack(V, {L, _, string})  -> {L, V};
unpack(V, {L, _, flag})    -> {L, V}.

opt_spec() ->
    [{profile, 'P', string},
     {logging, 'L', string},
     {dryrun,  'n', flag}].
