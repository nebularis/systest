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
-module(systest_shell).

-behaviour(systest_runner).
-behaviour(systest_log).

-include("systest.hrl").

%% systest_runner Exports
-export([dryrun/1, run/1]).
-export([start_log/0, write_log/4]).

%%
%% Public API
%%

dryrun(_RunSpec) ->
    ok.

run(RunSpec) ->
    {ok, Pid} = systest_standalone:start(),
    MRef = erlang:monitor(process, Pid),
    case systest_standalone:run(Pid, RunSpec) of
        ok ->
            spawn(fun user_drv:start/0),
            receive
                {'DOWN', MRef, _, _, Reason} ->
                    {error, {unknown, Reason}};
                {_Ref, {finished, Pid, Result}} ->
                    Result;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    {error, {died, Reason}};
                Other ->
                    {error, Other}
            end;
        {finished, Pid, Result} ->
            Result
    end.

start_log() ->
    ok = systest_log:start(shell, ?MODULE, user).

%%
%% systest_log callback API!
%%

write_log(EvId, user, What, Args) ->
    io:format("[~p]  " ++ What, [EvId|Args]).
