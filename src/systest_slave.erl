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
-module(systest_slave).

-behaviour(systest_proc).

-export([init/1, handle_stop/2, handle_kill/2]).
-export([handle_status/2, handle_interaction/3,
         handle_msg/3, terminate/3]).

-include("systest.hrl").

-import(systest_log, [log/2, log/3]).

-record(state, { runstate='running' :: atom() }).

%%
%% systest_proc API
%%

init(NI=#proc{host=Host, name=Name, config=Config}) ->
    VmArgs = systest_config:eval("flags.start", Config,
                    [{callback, {proc, fun systest_proc:get/2}},
                     {return, value}]),
    HostName = systest_env:qname(Host),
    on_start(NI, slave:start_link(HostName, Name, VmArgs)).

%% @doc handles interactions with the proc.
%% handle_interaction(Data, Proc, State) -> {reply, Reply, NewProc, NewState} |
%%                                          {reply, Reply, NewState} |
%%                                          {stop, Reason, NewProc, NewState} |
%%                                          {stop, Reason, NewState} |
%%                                          {NewProc, NewState} |
%%                                          NewState.
handle_interaction({M, F, Argv}, #proc{id=Id}, State) ->
    {reply, rpc:call(Id, M, F, Argv), State};
handle_interaction(_Data, _Proc, State) ->
    {reply, ignored, State}.

%% @doc handles a status request from the server.
%% handle_status(Proc, State) -> {reply, Reply, NewProc, NewState} |
%%                               {reply, Reply, NewState} |
%%                               {stop, NewProc, NewState}.
handle_status(Proc, State) ->
    case net_adm:ping(systest_proc:get(id, Proc)) of
        pong ->
            {reply, up, State};
        pang ->
            {reply, down, State#state{runstate=rpc_down}}
    end.

%% @doc handles a kill instruction from the server.
%% handle_kill(Proc, State) -> {NewProc, NewState} |
%%                             {stop, NewProc, NewState} |
%%                             NewState.
handle_kill(#proc{os_pid=OsPid}, State) ->
    systest:sigkill(OsPid),
    State#state{runstate=killed}.

%% @doc handles a stop instruction from the server.
%% handle_stop(Proc, State) -> {NewProc, NewState} |
%%                             {stop, NewProc, NewState} |
%%                             {rpc_stop, {M,F,A}, NewState} |
%%                             NewState.
handle_stop(Proc, State) ->
    slave:stop(systest_proc:get(id, Proc)),
    {stop, Proc, State#state{runstate=stopped}}.

%% @doc handles generic messages from the server.
%% handle_msg(Msg, Proc, State) -> {reply, Reply, NewProc, NewState} |
%%                                 {reply, Reply, NewState} |
%%                                 {stop, Reason, NewProc, NewState} |
%%                                 {stop, Reason, NewState} |
%%                                 {NewProc, NewState} |
%%                                 NewState.
handle_msg({nodedown, NodeId}, #proc{id=NodeId}, State=#state{runstate=RS}) ->
    ShutdownType = case RS of
                       killed  -> normal;
                       stopped -> normal;
                       _       -> down
                   end,
    {stop, ShutdownType, State};
handle_msg(Info, Proc, State=#state{runstate=RS}) ->
    log({framework, systest_proc:get(id, Proc)},
        "[~p] Ignoring Info Message:  ~p~n"
        "State:                       ~p~n",
        [?MODULE, Info, RS]),
    State.

terminate(Reason, Proc, _State) ->
    log({framework, systest_proc:get(id, Proc)},
        "Terminating due to ~p~n", [Reason]).

%%
%% Private API
%%

on_start(NI, {ok, Node}) ->
    OsPid = rpc:call(Node, os, getpid, []),
    erlang:monitor_node(Node, true),
    NI2 = NI#proc{os_pid=OsPid, id=Node},
    {ok, NI2, #state{runstate=running}};
on_start(_, Error) ->
    Error.

