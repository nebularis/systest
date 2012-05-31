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
-module(systest_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-record(state, {
    auto_start = false      :: boolean(),
    data       = dict:new() :: dict()
}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
  systest.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(systest, Opts) ->
    case application:start(systest, permanent) of
        {error, {already_started, systest}} -> systest:reset();
        {error, _Reason}=Err                -> Err;
        ok                                  -> ok
    end,
    {ok, #state{auto_start=application:get_env(systest, auto_start)}}.

%% @doc Called before init_per_suite is called, this code might start a
%% cluster, if one is configured for this suite.
pre_init_per_suite(Suite, Config, State) ->
    {Config, State}.


