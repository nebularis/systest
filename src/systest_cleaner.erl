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
-module(systest_cleaner).

-record(state, {killer, targets, victims}).

-behaviour(gen_server).

%% API Exports

-export([start/1, kill/1, kill/2]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%
%% Public API
%%

start(Killer) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Killer], []).

kill(Targets) ->
    kill(Targets, infinity).

kill(Targets, Timeout) ->
    gen_server:call(?MODULE, {kill, Targets}),
    receive
        {ok, Targets} -> ok
    after Timeout     -> ok
    end.

%%
%% OTP gen_server API
%%

init([Killer]) ->
    process_flag(trap_exit, true),
    {ok, #state{killer=Killer,
                targets=queue:new(),
                victims=[]}}.

handle_call({kill, Targets}, From,
            State=#state{targets=KillQ, killer=Kill}) ->
    [dispose(Pid, Kill) || Pid <- Targets],
    Q2 = queue:snoc(KillQ, {From, Targets}),
    {noreply, State#state{targets=Q2}};
handle_call(_Msg, _From, State) ->
    {reply, {error, noapi}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _},
            State=#state{targets=Targets, victims=Victims}) ->
    {Client, Nodes} = queue:head(Targets),
    case lists:member(Pid, Nodes) of
        false ->
            {stop, {unexpected_exit, Pid}, State};
        true ->
            T2 = lists:delete(Pid, Nodes),
            V2 = [Pid|Victims],
            case T2 of
                [] ->
                    gen_server:reply(Client, {ok, V2}),
                    Q = queue:tail(Targets),
                    case queue:is_empty(Q) of
                        true  ->
                            {stop, normal, State};
                        false ->
                            {noreply, State#state{targets=Q, victims=[]}}
                    end;
                _More ->
                    {noreply, State#state{targets=Targets, victims=V2}}
            end
    end.

dispose(Pid, Kill) ->
    catch link(Pid),
    Kill(Pid).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

