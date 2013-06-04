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
%% @hidden
%% ----------------------------------------------------------------------------
-module(systest_cleaner).

-record(state, {killer, targets, victims}).

-behaviour(gen_server).

%% API Exports

-export([start_link/1, start/1, start_permanent/1,
         kill/2, kill/3, kill_wait/2, kill_wait/3]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%
%% Public API
%%

start_permanent(Killer) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Killer], []).

start_link(Killer) ->
    gen_server:start_link(?MODULE, [Killer], []).

start(Killer) ->
    gen_server:start(?MODULE, [Killer], []).

kill_wait(Targets, Killer) ->
    kill_wait(Targets, Killer, infinity).

kill_wait([], _Killer, _Timeout) ->
    no_targets;
kill_wait(Targets, Killer, Timeout) ->
    {ok, Server} = start(Killer),
    kill(Targets, Server, Timeout).

kill(Targets, Server) ->
    kill(Targets, Server, infinity).

kill([], _Server, _Timeout) ->
    no_targets;
kill(Targets, Server, Timeout) ->
    MRef = erlang:monitor(process, Server),
    wait = gen_server:call(Server, {kill, Targets}, Timeout),
    Result = receive
                 {_Ref, {ok, Killed}} ->
                     check_length(Targets, Killed);
                 {'EXIT', Server, normal} ->
                     %% is it *possible* that the 'EXIT' overtakes the reply!
                     check_length(Targets, dead_pids(Targets));
                 {'DOWN', MRef, process, Server, Reason} ->
                     {error, Reason}
             after Timeout ->
                     {error, timeout, dead_pids(Targets)}
             end,
    erlang:demonitor(MRef, [flush]),
    Result.

%%
%% Internal API
%%

dead_pids(Targets) ->
    lists:filter(fun erlang:is_process_alive/1, Targets).

check_length(Targets, Killed) ->
    case length(Killed) =:= length(Targets) of
        true  -> ok;
        false -> {error, {killed, Targets}}
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
    link_and_kill(Targets, Kill),
    Q2 = queue:snoc(KillQ, {From, Targets}),
    {reply, wait, State#state{targets=Q2}};
handle_call(_Msg, _From, State) ->
    {reply, {error, noapi}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason},
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
                    RemainingQueue =
                        queue:cons({Client, T2}, queue:tail(Targets)),
                    {noreply, State#state{targets=RemainingQueue, victims=V2}}
            end
    end;
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State}.

link_and_kill(Pids, Kill) ->
    [erlang:monitor(process, P) || P <- Pids],
    [spawn_link(fun() -> Kill(P) end) || P <- Pids].

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

