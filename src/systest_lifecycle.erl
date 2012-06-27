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
-module(systest_lifecycle).

-behaviour(gen_fsm).

-export([start/1, start/2, start_link/2, start/3, start_link/3]).
-export([stop/1, stop/2]).
-export([restart_node/2, restart_node/3]).
-export([list_nodes/1, check_config/2, status/1]).
-export([print_status/1, log_status/1]).
-export([node_names/1, node_pids/1]).

%% OTP gen_server Exports

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/2, terminate/2, code_change/3]).

-include("systest.hrl").

-record(lifecycle, {id, descendants, hooks}).

%%
%% Public API
%%

start() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

starting(What, Scope, Data) ->
    gen_fsm:send_all_state_event(?MODULE,

%%
%% gen_fsm API
%%

init([Id, Data]) ->
    {ok, started, #lifecycle{id=Id,
                             hooks=Data,
                             descendants=[]}}.
init(_) ->
    {ok, preparing, #lifecycle{}}.

handle_event({starting, What, Context, Data},
             State, Fsm=#lifecycle{top=self(), descendants=[]}) ->
    NextState = starting_state_name(What),
    upgrade(Context, Data),
    {next_state, NextState, Fsm};
handle_event({starting, What, Context, Data},
             State, Fsm=#lifecycle{descendants=Desc}) ->


starting_cluster({upgrade, Context, Data}, Fsm) ->
    {next_state, starting_cluster, start_child(Fsm, Context, Data)}.

handle_sync_event({Tag, EventName}, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

send(Pid, Event) ->
    ok = gen_fsm:send_event(Pid, Event).

start_child(Fsm=#lifecycle{descendants=Desc}, Context, Data) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [Context, Data], []),
    Fsm#lifecycle{descendants=[Pid|Desc]}.

upgrade(Context, Data) ->
    send(self(), {upgrade, Context, Data}).

starting_state_name(Name) ->
    next_state_name("starting", Name).

next_state_name(Prefix, Name) ->
    list_to_atom(string:join([Prefix, atom_to_list(Name)], "_")).

