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
%% @hidden (for now)
%% states:
%% idle    -> start
%% start   -> locked(starting)
%% stop    -> locked(stopping)
%% locked  -> started, stopped
%% started -> stop
%% stopped -> start
%% any     -> locked(permanent)
%% ----------------------------------------------------------------------------
-module(systest_resource).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("systest.hrl").

-define(IS_LOCKSTATE(S),
        S == starting orelse
        S == stopping orelse
        S == perm_locked).

-define(IS_CLEAN_SHUTDOWN(Reason),
        Reason == normal orelse
        Reason == shutdown).

%% TODO: remove this....
-compile(export_all).

-record(state, {
    resource             :: resource(),
    insulator            :: pid(),
    child                :: pid(),
    clients = dict:new() :: dict()
}).

-type resource() :: #resource{}.
-export_types([resource/0]).

%%
%% systest_resource behaviour API
%%

-callback activate(resource()) ->
    {'error', Reason :: term()} |
    {ok, Ref :: pid() | term()} |
    'ok'.

-callback deactivate(resource()) ->
    ExitStatus::term().

%%
%% Public API
%%

create(Scope, Id, Handler, InitState) ->
    start_link([Scope, Id, Handler, InitState]).

start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

activate(Ref) ->
    gen_fsm:sync_send_event(Ref, activate).

deactivate(Ref) ->
    gen_fsm:sync_send_event(Ref, deactivate).

current_state(Ref) ->
    gen_fsm:sync_send_all_state_event(Ref, check_state).

%%
%% gen_fsm API
%%

init([Scope, Id, Handler, InitState]) ->
    process_flag(trap_exit, true),
    Resource = #resource{scope=Scope,
                         identity=Id,
                         handler=Handler,
                         init_state=InitState},
    {ok, idle, #state{resource=Resource}}.

idle(activate, From, SD=#state{resource=Res,
                               clients=Clients}) ->
    Timeout = systest_lock_timer:get_max_lock_timeout(),
    Options = [{timeout, Timeout}],
    Self = self(),
    Resource = Res#resource{options=Options},
    InsulatorPid =
        spawn_link(fun() -> insulator(Self, activate, Resource) end),

    Clients2 = dict:store(starting, From, Clients),

    %% It *might* seem simpler to use a built in function like
    %% gen_fsm:send_event_after(Timeout, max_lock_timeout)
    %% rather that systest_lock_timer - but the latter would
    %% require that handle the max_lock_timeout event in every
    %% state! This is much simpler.
    systest_lock_timer:acquire(),
    {reply, ok, starting, SD#state{insulator=InsulatorPid,
                                   clients=Clients2}}.

starting({activate, Ref}, SD=#state{resource=Resource,
                                    insulator=InsulatorPid}) ->
    systest_lock_timer:release(),
    systest_log:framework("resource ~p started~n",
                          [Resource#resource.identity]),
    Id = Resource#resource.identity,
    Clients = reply(SD, starting, {started, Id, Ref}),

    systest_utils:throw_unless(is_process_alive(InsulatorPid),
                               "insulator pid died unexpectedly"),
    InsulatorPid ! stop,

    case Ref of
        Pid when is_pid(Pid) ->
            erlang:monitor(process, Ref),
            {next_state, active,
             SD#state{child=Ref, clients=Clients,
                      insulator={stopped, InsulatorPid}}};
        Other ->
            {next_state, active,
             SD=#state{child={noproc, Other}, clients=Clients,
                       insulator={stopped, InsulatorPid}}}
    end.

active({exit, Pid, Reason}, State=#state{child=Pid})
  when Reason == normal orelse
       Reason == shutdown ->
    {stop, normal, State};
active({exit, Pid, Reason}, State=#state{child=Pid}) ->
    {stop, {resource_exit, Reason}, State}.

active(deactivate, From, SD=#state{resource=Res,
                                   clients=Clients}) ->
    Timeout = systest_lock_timer:get_max_lock_timeout(),
    Options = [{timeout, Timeout}],
    Self = self(),
    Resource = Res#resource{options=Options},
    InsulatorPid =
        spawn_link(fun() -> insulator(Self, deactivate, Resource) end),

    Clients2 = dict:store(stopping, From, Clients),

    systest_lock_timer:acquire(),
    {reply, ok, stopping, SD#state{insulator=InsulatorPid,
                                   clients=Clients2}}.

stopping({deactivate, Reason}, SD) when ?IS_CLEAN_SHUTDOWN(Reason) ->
    Clients = tagged_reply(stopped, Reason, stopping, SD),
    {next_state, idle, SD#state{clients=Clients}};
stopping({deactivate, Reason}, SD=#state{resource=Resource}) ->
    %% when an unexpected deactivation ???????
    Id = Resource#resource.identity,
    Clients = tagged_reply(stopped, Reason, stopping, SD),
    {stop, {stopped, Id, Reason}, SD#state{clients=Clients}};
%% if the insulator dies before we got the exit signal from the
%% child (resource) - this is an error condition of sorts, but
%% should we actually shut down? Well it depends on
%% the exit reason of the child....
stopping({exit, Pid, Reason}, SD=#state{child=Pid})
  when ?IS_CLEAN_SHUTDOWN(Reason) ->
    %% a clean shutdown after we we're asked to stop means
    %% that we're good to go and need to release the timeout lock
    systest_lock_timer:release(),
    Clients = tagged_reply(stopped, Reason, stopping, SD),
    {next_state, idle, SD#state{child=undefined,
                                clients=Clients}};
stopping({exit, Pid, Reason}, SD=#state{child=Pid,
                                        resource=Resource})
  when not( ?IS_CLEAN_SHUTDOWN(Reason) ) ->
    %% a non-normal shutdown during stopping is a failure though
    %% and we don't want to lock up this instance as we don't
    %% really know what's gone wrong - let our parent decide by
    %% stopping and allowing them to choose how to handle it
    Clients = tagged_reply(stopped, Reason, stopping, SD),
    Id = Resource#resource.identity,
    {stop, {stopped, Id, Reason}, SD#state{clients=Clients}}.

%% TODO: child-down should trigger a change even when we're
%% permanently locked - probably we should go back to idle!?
perm_locked(_Event, State) ->
    {next_state, perm_locked, State}.

perm_locked(_Event, _From, State) ->
    {reply, {error, {locked, permanent}}, locked, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(check_state, _From, S=perm_locked, SD) ->
    {reply, {locked, permanent}, S, SD};
handle_sync_event(check_state, _From, State, SD)
  when ?IS_LOCKSTATE(State) ->
    {reply, {locked, State}, State, SD};
handle_sync_event(check_state, _From, StateName, State) ->
    {reply, StateName, StateName, State}.

handle_info({'DOWN', _MRef, process, Pid, Reason},
            StateName, StateData=#state{child=Child}) when Pid == Child ->
    %% child is down - we handle this differently for various states...
    gen_fsm:send_event(self(), {exit, Pid, Reason}),
    {next_state, StateName, StateData};
handle_info({'EXIT', Pid, _Reason},
            StateName,
            State=#state{insulator={stopped, InsulatorPid}})
  when Pid == InsulatorPid ->
    %% we've already *seen* the insulator stopped, so there's
    %% not much state adjustment necessary - now we've seen an
    %% 'EXIT' signal we can stop tracking the pid
    {next_state, StateName, State#state{insulator=stopped}};
handle_info({'EXIT', Pid, Reason},
            StateName,
            State=#state{insulator=InsulatorPid,
                         resource=Resource,
                         child=undefined})
  when ?IS_LOCKSTATE(StateName) andalso
       Pid == InsulatorPid ->
    %% the insulator died before returning a child pid
    systest_log:framework("~p insulator process for ~p "
                          "failed whilst locked: ~p~n",
                          [?MODULE, Resource#resource.identity, Reason]),
    case StateName of
        perm_locked ->
            systest_lock_timer:release(),
            {next_state, StateName,
             State#state{insulator=died}};
        _Other ->
            {stop, {insulator_failed, StateName, Reason}, State}
    end;
handle_info({'EXIT', Pid, Reason},
            StateName,
            State=#state{insulator=InsulatorPid,
                         resource=Resource,
                         child=Child})
 when ?IS_LOCKSTATE(StateName) andalso
       Pid == InsulatorPid ->
    %% the insulator has died whilst we are still locked!
    %% the startup and shutdown behaviour callbacks shouldn't
    %% really do that, but code can crash right, and we need to
    %% see a 'DOWN' for the child before we unlock and go back
    %% into an idle state
    systest_log:framework("~p insulator process for ~p "
                          "died whilst locked: ~p~n"
                          "will wait for ~p to shutdown~n",
                          [?MODULE, Resource#resource.identity,
                           Reason, Child]),
    {next_state, StateName, State#state{insulator=died}};
%% TODO: bad insulator shutdown.....
handle_info(max_lock_timeout, StateName, SD=#state{resource=Resource})
  when ?IS_LOCKSTATE(StateName) ->
    %% we've exceeded max_lock_timeout, so we become permanently locked
    %% and nothing any client does (short of killing the gen_fsm process
    %% and restarting another one) can get around this constraint
    Id = Resource#resource.identity,
    Clients = reply(SD, StateName, {max_lock_timeout, Id, StateName}),
    {next_state, perm_locked, SD#state{clients=Clients}};
handle_info(max_lock_timeout, State, SD) ->
    %% it is entirely possible that the cancellation we send to
    %% systest_lock_timer doesn't arrive fast enough, that the timer
    %% is not cancelled, and we subsequently receive the max-lock-timeout
    %% signal despite all our efforts. In this situation, we can happily
    %% ignore the timeout
    {next_state, State, SD};
handle_info(Info, StateName, State) ->
    {stop, {unexpected_info, StateName, Info}, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Private API
%%

tagged_reply(Tag, Data, State, StateData=#state{resource=Resource}) ->
    Id = Resource#resource.identity,
    reply(StateData, State, {Tag, Id, Data}).

reply(SD, Trigger, Reply) ->
    case dict:find(Trigger, SD#state.clients) of
        error -> SD;
        {ok, From} -> gen_fsm:reply(From, Reply),
                      dict:erase(Trigger, SD#state.clients)
    end.

insulator(Parent, StateEvent, Resource=#resource{handler=Mod}) ->
    process_flag(trap_exit, true),
    case erlang:apply(Mod, StateEvent, [Resource]) of
        {ok, Pid} when is_pid(Pid) ->
            gen_fsm:send_event(Parent, {StateEvent, Pid});
        Other ->
            gen_fsm:send_event(Parent, {StateEvent, Other})
    end,
    receive
        stop -> ok
    end.
