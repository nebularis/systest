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
-module(systest_lock_timer).

-behaviour(gen_server).

-export([start_link/0, acquire/0, release/0]).
-export([set_max_lock_timeout/1, get_max_lock_timeout/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%
%% Public API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

acquire() ->
    gen_server:cast(?MODULE, {start_timeout, self()}).

release() ->
    gen_server:cast(?MODULE, {cancel_timeout, self()}).

set_max_lock_timeout(Val) ->
    gen_server:call(?MODULE, {set, systest_utils:time_to_ms(Val)}).

get_max_lock_timeout() ->
    gen_server:call(?MODULE, get).

%%
%% OTP gen_server API
%%

init([]) ->
    init([infinity]);
init([MaxLockTimeout]) ->
    {ok, {MaxLockTimeout, []}}.

handle_call({set, MaxLockTimeout}, _From, {Prev, Clients}) ->
    {reply, {ok, Prev}, {MaxLockTimeout, Clients}};
handle_call(get, _From, {MaxLockTimeout, _}=State) ->
    {reply, MaxLockTimeout, State}.

handle_cast(_, {infinity, _}=State) ->
    {noreply, State};
handle_cast({start_timeout, Sender}, {MaxLockTimeout, Clients}) ->
    erlang:monitor(process, Sender),
    TRef = erlang:send_after(MaxLockTimeout, Sender, max_lock_timeout),
    {noreply, {MaxLockTimeout, [{Sender, TRef}|Clients]}};
handle_cast({cancel_timeout, Sender}, {T, Clients}=State) ->
    case lists:keytake(Sender, 1, Clients) of
        false ->
            {noreply, State};
        {value, {_, TRef}, Remaining} ->
            erlang:cancel_timer(TRef),
            {noreply, {T, Remaining}}
    end.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, {T, Clients}) ->
    %% timers started with erlang:send_after/3 cancel themselves when
    %% the dest pid goes away, but we need to clear out our list of
    %% clients as well, so as to avoid building up an unnecessarily
    %% large overhead when traversing the list after pids die
    {noreply, {T, lists:keydelete(Pid, 1, Clients)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


