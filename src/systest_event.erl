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
-module(systest_event).

-include("systest.hrl").
-include_lib("common_test/include/ct_event.hrl").

-export([init/1
        ,handle_event/2
        ,handle_call/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

init([]) ->
    {ok, []}.

handle_event(#event{name=tc_done,
                    data={Suite, end_per_suite, _Result}}, State) ->
    systest_watchdog:force_stop(Suite),
    {ok, State};
handle_event(#event{name=tc_done,
                    data={_Suite, {end_per_group, Group, _}, _Result}},
             State) ->
    systest_watchdog:force_stop(Group),
    {ok, State};
handle_event(#event{name=tc_done,
                    data={_Suite, Func, _Result}},
            State) when is_atom(Func) ->
    systest_watchdog:force_stop(Func),
    {ok, State};
handle_event(_Message, State) ->
    {ok, State}.

%%
%% @private
%%
handle_call(_, State) ->
    {ok, ignored, State}.

%%
%% @private
%%
handle_info(_Info, State) ->
    {ok, State}.

%%
%% @private
%%
terminate(_Reason, _State) ->
    ok.

%%
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

