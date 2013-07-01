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
-module(systest_proc).

-include("systest.hrl").

-type proc_ref() :: pid().
-type activity_state() :: 'not_started' |
                          'running'     |
                          'stopped'     |
                          'killed'      |
                          'down'.
-type proc_info() :: #proc{}.
-export_type([proc_info/0, activity_state/0, proc_ref/0]).

-export([behaviour_info/1]).
-export([make_proc/3]).
-export([interact/2]).
-export([proc_id/2, proc_data/1, proc_data/2]).
-export([user_data/1, user_data/2]).
-export([start/1, start/3, stop/1, kill/1, activate/1]).
-export(['kill -9'/1, stop_and_wait/1, kill_and_wait/1]).
-export([sigkill/1, kill_after/2, kill_after/3]).
-export([shutdown_and_wait/2, status/1, activity_state/1]).
-export([joined_sut/3]).
-export([status_check/1]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-import(systest_log, [log/2, log/3,
                      framework/2, framework/3]).
-import(systest_utils, [safe_call/3, call/2]).

-ifdef(TEST).
-export([proc_config/2]).
-endif.

-exprecs_prefix([operation]).
-exprecs_fname(["record_", prefix]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records([proc]).

%% our own internal state is separate from that of any handler...
-record(state, {
    proc            :: proc_info(),
    handler         :: module(),
    handler_state   :: term(),
    activity_state  :: activity_state()
}).

%%
%% Public API
%%

-spec behaviour_info(term()) -> list({atom(), integer()}) | 'undefined'.
behaviour_info(callbacks) ->
    [{init,               1},
     {handle_stop,        2},
     {handle_kill,        2},
     {handle_status,      2},
     {handle_interaction, 3},
     {handle_msg,         3},
     {terminate,          3}];
behaviour_info(_) ->
    undefined.

-spec make_proc(atom(), atom(), systest_config:config()) -> proc_info().
make_proc(Scope, Proc, Config) ->
    Settings = systest_config:get_static(settings),
    make_proc([{ct, Config},Settings] ++ Config ++ proc_config(Scope, Proc)).

-spec start(atom(), atom(),
            list(tuple(atom(), term()))) -> {'ok', pid()} | {'error', term()}.
start(Scope, Proc, Config) ->
    start(make_proc(Scope, Proc, Config)).

-spec start(proc_info()) -> {'ok', pid()} | {'error', term()}.
start(ProcInfo=#proc{handler=Handler, host=Host,
                     name=Name, config=BaseConf}) ->
    %% are there hidden traps here, when (for example) we're running
    %% embedded in an archive/escript or similarly esoteric situations?
    %% TODO: perhaps catch(Handler:id(ProcInfo)) would be safer!?
    code:ensure_loaded(Handler),

    Id = case erlang:function_exported(Handler, id, 1) of
             true  -> set([{id, Handler:id(ProcInfo)}], ProcInfo);
             false -> set([{id, proc_id(Host, Name)}], ProcInfo)
         end,
    NI = set([{config,[{proc, Id}|BaseConf]}], Id),

    framework("starting ~p on ~p~n", [Name, Host]),
    gen_server:start_link(?MODULE, [NI], []).

-spec activate(proc_ref()) -> ok.
activate(ProcRef) ->
    call(ProcRef, start).

-spec stop(proc_ref()) -> ok.
stop(ProcRef) ->
    gen_server:cast(ProcRef, stop).

-spec kill(proc_ref()) -> ok.
kill(ProcRef) ->
    gen_server:cast(ProcRef, kill).

-spec('kill -9'/1 :: (proc_ref()) -> 'ok').
'kill -9'(ProcRef) ->
    sigkill(ProcRef).

-spec sigkill(proc_ref()) -> 'ok'.
sigkill(ProcRef) ->
    framework(
      "[WARNING] using SIGKILL is *NOT*"
      " guaranteed to work with all proc types!~n", []),
    gen_server:cast(ProcRef, sigkill).

-spec kill_after(integer(), proc_ref()) -> 'ok'.
kill_after(TimeoutMs, ProcRef) ->
    kill_after(TimeoutMs, ProcRef, kill).

-spec kill_after(integer(), proc_ref(), atom()) -> 'ok'.
kill_after(TimeoutMs, ProcRef, Killer) ->
    {ok, _TRef} = timer:apply_after(TimeoutMs, ?MODULE, Killer, [ProcRef]),
    ok.

-spec stop_and_wait(proc_ref()) -> 'ok'.
stop_and_wait(ProcRef) ->
    shutdown_and_wait(ProcRef, fun stop/1).

-spec kill_and_wait(proc_ref()) -> 'ok'.
kill_and_wait(ProcRef) ->
    shutdown_and_wait(ProcRef, fun kill/1).

-spec status(proc_ref()) -> 'up' | {'down', term()}.
status(ProcRef) ->
    safe_call(ProcRef, status, {down, noproc}).

-spec activity_state(proc_ref()) -> activity_state().
activity_state(ProcRef) ->
    {ok, State} = call(ProcRef, state),
    State.

-spec interact(proc_ref(), term()) -> term().
interact(ProcRef, InputData) ->
    call(ProcRef, {interaction, InputData}).

-spec proc_data(proc_ref()) -> [{atom(), term()}].
proc_data(ProcRef) ->
    safe_call(ProcRef, proc_info_list, [{owner, ProcRef}]).

-spec proc_data(proc_ref(), atom()) -> term().
proc_data(ProcRef, Field) ->
    call(ProcRef, {proc_info_get, Field}).

-spec user_data(proc_ref()) -> [{atom(), term()}].
user_data(ProcRef) ->
    call(ProcRef, user_data).

-spec user_data(proc_ref(), term()) -> 'ok'.
user_data(ProcRef, Data) ->
    call(ProcRef, {user_data, Data}).

-spec joined_sut(proc_ref(), pid(), [{atom(), pid()}]) -> 'ok'.
joined_sut(ProcRef, SutRef, SiblingProcs) ->
    ok = call(ProcRef, {joined, SutRef, SiblingProcs}).

%% NB: this *MUST* run on the client
shutdown_and_wait(Owner, ShutdownOp) when is_pid(Owner) ->
    case (Owner == self()) orelse not(is_process_alive(Owner)) of
        true  -> ok;
        false -> framework("waiting for ~p to exit~n", [Owner]),
                 systest_cleaner:kill_wait([Owner], ShutdownOp)
    end,
    framework("shutdown_and_wait complete~n", []).

%%
%% Handler facing API
%%
status_check(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong  -> up;
        Other -> {down, Other}
    end.

%%
%% OTP gen_server API
%%

init([ProcInfo=#proc{auto_start=false}]) ->
    {ok, #state{proc=ProcInfo, activity_state=not_started}};
init([ProcInfo]) ->
    do_start(ProcInfo).

handle_call(state, _From, State=#state{activity_state=ActivityState}) ->
    {reply, {ok, ActivityState}, State};
handle_call(Msg, From, State) ->
    handle_msg(Msg, State, From).

handle_cast(Msg, State) ->
    handle_msg(Msg, State).

handle_info(Info, State) ->
    handle_msg(Info, State).

terminate(_Reason, #state{activity_state=not_started}) ->
    ok;
terminate(Reason, #state{proc=Proc,
                         handler=Mod,
                         handler_state=ModState}) ->
    ok = Mod:terminate(Reason, Proc, ModState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

do_start(ProcInfo=#proc{handler=Callback, cover=Cover}) ->
    process_flag(trap_exit, true),
    case catch( apply(Callback, init, [ProcInfo]) ) of
        {ok, NI2, HState} when is_record(NI2, proc) ->

            Id = get(id, NI2),

            LogBase = systest_env:default_log_dir(get(config, NI2)),
            case systest_log:activate_logging_subsystem(process, Id, LogBase) of
                {error, _} ->
                    framework("per-process logging is disabled~n", []);
                {ok, Dest} ->
                    framework("per-process logging to ~p~n", [Dest])
            end,

            %% NB: look at branch 'cover' for details
            %% of how this will eventually look
            case Cover of
                true ->
                    systest_cover:start_cover(Id);
                _ ->
                    ok
            end,

            %% TODO: validate that these succeed and shutdown when they don't
            case NI2#proc.apps of
                []   -> ok;
                Apps -> [setup(NI2, App, HState) || App <- Apps]
            end,

            NI3 = NI2#proc{owner=self()},
            State = #state{proc=NI3,
                           activity_state=running,
                           handler=Callback,
                           handler_state=HState},

            %% TODO: validate that these succeed and shutdown when they don't
            case NI2#proc.on_start of
                []   -> {ok, State};
                Xtra -> try
                            {NI4, _} = lists:foldl(fun apply_startup/2,
                                                   {NI3, HState}, Xtra),
                            {ok, State#state{proc=NI4}}
                        catch
                            _:{stop, Reason} -> {stop, Reason}
                        end
            end;
        {error, Err} ->
            {stop, Err};
        Error ->
            %% TODO: do NOT rely on callbacks returning a proper gen_server
            %% init compatible response tuple - construct this for them....
            {stop, Error}
    end.

proc_id(Host, Name) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

apply_startup(Item, {Proc, _}=ProcState) ->
    try
        apply_hook(on_start, Item, ProcState)
    catch
        _:Reason ->
            systest_log:log("ERROR: ~p encountered failures whilst processing "
                            "hook 'on_start' - see the log(s) for details~n",
                            [get(name, Proc)]),
            throw({stop, Reason})
    end.

apply_hook(Hook, Item, {Proc, HState}) ->
    framework("apply hook ~p to ~p~n",
              [Hook, get(name, Proc)]),
    case systest_hooks:run(Proc, Item, HState) of
        {upgrade, Proc2} ->
            {Proc2, HState};
        {store, State} ->
            StateL = case is_list(State) of
                         true  -> State;
                         false -> [State]
                     end,
            Existing = get(user, Proc),
            {set([{user, StateL ++ Existing}], Proc), HState};
        {write, Loc, Data} ->
            framework(get(id, Proc),
                      "~p (argv = ~p, state-update = [~p => ~p])~n",
                      [Hook, Item, Loc, Data]),
            {systest_proc:set([{Loc, Data}], Proc), HState};
        Other ->
            framework(get(id, Proc),
                "~p (argv = ~p, response = ~p)~n",
                [Hook, Item, Other]),
            {Proc, HState}
    end.

on_join(Proc, Sut, Procs, Hooks) ->
    SutRef = systest_sut:get(id, Sut),
    log({framework, get(id, Proc)},
        "process has joined system under test: ~p~n", [SutRef]),
    %% TODO: this is COMPLETELY inconsistent with the rest of the
    %% hooks handling - this whole area needs some serious tidy up
    try
        {Proc2, _} = lists:foldl(fun({Where, M, F}, Acc) ->
                                         apply_hook(on_join,
                                                    {Where, M, F,
                                                     [SutRef, Procs]},
                                                    Acc);
                                    ({Where, M, F, A}, Acc) ->
                                         apply_hook(on_join,
                                                    {Where, M, F,
                                                     [SutRef, Procs|A]},
                                                    Acc);
                                    (What, Acc) ->
                                         throw({What, Acc})
                                 end, {Proc, undefined}, Hooks),
        Proc2
    catch _:Err ->
            {error, Err}
    end.

%% TODO: migrate this to systest_hooks....

interact(Proc=#proc{id=NodeId},
         {eval, Where, Mod, Func, Args}, _HState) ->
    Argv = lists:reverse(lists:foldl(fun proc_interact/2, {Proc, []}, Args)),
    case Where of
        local ->
            apply(Mod, Func, Argv);
        remote ->
            rpc:call(NodeId, Mod, Func, Argv);
        Id when is_atom(Id) ->
            rpc:call(Id, Mod, Func, Argv)
    end;
interact(Proc, {local, Mod, Func, Args}, _) ->
    apply(Mod, Func, [Proc|Args]);
interact(#proc{id=Id}, {remote, Mod, Func, Args}, _) ->
    rpc:call(Id, Mod, Func, [self()|Args]);
interact(#proc{id=Node}, {Mod, Func, Args}, _) ->
    rpc:call(Node, Mod, Func, Args);
interact(NI=#proc{handler=Handler}, Inputs, HState) ->
    Handler:handle_interaction(Inputs, NI, HState).

proc_interact({plain_call, M, F, A}, {_Proc, Acc}) ->
    [apply(M, F, A)|Acc];
proc_interact({call, M, F, A}, {Proc, Acc}) ->
    [apply(M, F, [Proc|A])|Acc];
proc_interact({proc, Field}, {Proc, Acc}) ->
    [get(Field, Proc)|Acc];
proc_interact(Term, {_, Acc}) ->
    [Term|Acc].

handle_msg(Msg, State) ->
    handle_msg(Msg, State, noreply).

handle_msg({joined, _Sut, _Procs},
            State=#state{activity_state=not_started}, _ReplyTo) ->
    framework("skipping on_join hook for ~p "
              "[activity_state = not_started]~n",
              [self()]),
    {reply, ok, State};
handle_msg({proc_info_get, Field}, State=#state{proc=Proc}, _ReplyTo) ->
    Value = get(Field, Proc),
    {reply, {ok, Value}, State};
handle_msg({user_data, Data}, State=#state{proc=Proc}, _ReplyTo) ->
    {reply, 'ok', State#state{proc=set([{user, Data}], Proc)}};
handle_msg(proc_info_list, State=#state{proc=Proc}, _ReplyTo) ->
    Attrs = systest_proc:info('proc', fields) -- [config],
    Info = [{K, get(K, Proc)} || K <- Attrs],
    {reply, Info, State};
handle_msg(user_data, State=#state{proc=Proc}, _ReplyTo) ->
    {reply, get(user, Proc), State};
%% NB: this section comprises the activity_state = not_started block
handle_msg(start, #state{proc=Proc, activity_state=not_started}, _) ->
    case do_start(Proc) of
        {stop, _}=Stop -> Stop;
        {ok, NewState} -> {reply, ok, NewState#state{activity_state=running}}
    end;
handle_msg(start, State, _) ->
    {reply, {error, already_started}, State};
handle_msg(Msg, State=#state{activity_state=not_started}, _ReplyTo)
                    when Msg == stop orelse
                         Msg == kill orelse
                         Msg == sigkill ->
    {stop, normal, State};
handle_msg(_, State=#state{activity_state=not_started}, _ReplyTo) ->
    {reply, {error, not_started}, State};
%% NB: every clause below *requires* that activity_state /= not_started
handle_msg({joined, Sut, Procs}, State=#state{proc=Proc}, _ReplyTo) ->
    framework(get(id, Proc),
              "proc on_join info: ~p~n", [Proc#proc.on_join]),
    case Proc#proc.on_join of
        []    -> {reply, ok, State};
        Hooks -> case on_join(Proc, Sut, Procs, Hooks) of
                     {error, Reason} ->
                         log("ERROR: ~p encountered failures whilst "
                             "processing hook 'on_join' - see "
                             "the log(s) for details~n",
                             [get(id, Proc)]),
                         {stop, Reason, State};
                     Proc2 ->
                         {reply, ok, State#state{proc=Proc2}}
                 end
    end;
%% down notifications
handle_msg({nodedown, NodeId},
           SvrState=#state{activity_state=State,
                           proc=#proc{id=ProcID}},
           _ReplyTo) when NodeId == ProcID ->
    ShutdownType = case State of
                       killed  -> normal;
                       stopped -> normal;
                       _       -> down
                   end,
    {stop, ShutdownType, SvrState};
handle_msg({nodedown, NodeId},
           State=#state{activity_state=running,
                        proc=#proc{id=ProcID}},
                        _ReplyTo) when NodeId == ProcID ->
    {stop, down, State#state{activity_state=down}};
%% instructions from clients
handle_msg(stop, State=#state{activity_state=stopped}, _) ->
    {noreply, State};
handle_msg(kill, State=#state{activity_state=stopped}, _) ->
    {noreply, State};
handle_msg(stop, State=#state{activity_state=killed}, _) ->
    {noreply, State};
handle_msg(stop, State=#state{proc=Proc, handler=Mod,
                              handler_state=ModState}, ReplyTo) ->
    case Proc#proc.on_stop of
        [] -> ok;
        %% TODO: consider whether this is structured correctly - it *feels*
        %% a little hackish - and perhaps having a supervising process deal
        %% with these 'interactions' would be better
        Shutdown ->
            [begin
                 framework(get(id, Proc),
                           "running on_stop hook: ~p~n",
                           [In]),
                 %% TODO: we should really enable timeouts here
                 framework(get(id, Proc),
                           "on_stop (response = ~p)~n",
                           [interact(Proc, In, ModState)])
             end || In <- Shutdown]
    end,
    handle_callback(stopping_callback(Mod, handle_stop, Proc,
                                      [Proc, ModState]),
                    State#state{activity_state=stopped}, ReplyTo);
handle_msg(kill, State=#state{proc=Proc, handler=Mod,
                              handler_state=ModState}, ReplyTo) ->
    handle_callback(stopping_callback(Mod, handle_kill, Proc,
                                      [Proc, ModState]),
                    State#state{activity_state=killed}, ReplyTo);
handle_msg(sigkill, State=#state{proc=Proc, handler=Mod,
                                 handler_state=ModState}, ReplyTo) ->
    handle_callback(stopping_callback(Mod, handle_msg, Proc,
                                      [sigkill, Proc, ModState]),
                    State#state{activity_state=killed}, ReplyTo);
handle_msg(status, State=#state{activity_state=stopped}, _ReplyTo) ->
    {reply, {stopping, stopped}, State};
handle_msg(status, State=#state{activity_state=killed}, _ReplyTo) ->
    {reply, {stopped, killed}, State};
handle_msg(status, State=#state{proc=Proc, handler=Mod,
                                handler_state=ModState}, ReplyTo) ->
    handle_callback(Mod:handle_status(Proc, ModState), State, ReplyTo);
handle_msg({interaction, _},
           State=#state{activity_state=ActivityState}, _)
                when ActivityState =:= killed orelse
                     ActivityState =:= stopped ->
    {reply, {error, stopping}, State};
handle_msg({interaction, InputData},
            State=#state{proc=Proc, handler=Mod,
                         handler_state=ModState}, ReplyTo) ->
    framework(get(id, Proc), "handle_interaction: ~p~n", [InputData]),
    handle_callback(Mod:handle_interaction(InputData,
                                           Proc, ModState), State, ReplyTo);
%% our catch-all, which defers to Mod:handler_state/3 to see if the
%% callback module knows what to do with Msg or not - this also allows the
%% handler an opportunity to decide how to deal with unexpected messages
handle_msg(Msg, State=#state{proc=Proc, handler=Mod,
                             handler_state=ModState}, ReplyTo) ->
    handle_callback(Mod:handle_msg(Msg, Proc, ModState), State, ReplyTo).

stopping_callback(Mod, Func, Proc, Args) ->
    case get(cover, Proc) of
        true ->
            Id = get(id, Proc),
            %% TODO: systest_cover needs to become a gen_server
            %% so that we can timeout on the call to stop remote cover
            systest_cover:stop_cover(Id);
        _ ->
            ok
    end,
    case apply(Mod, Func, Args) of
        {stop, NewProc, NewModState} ->
            {stopped, NewProc, NewModState};
        Other ->
            Other
    end.

handle_callback(CallbackResult,
                State=#state{proc=Proc,
                             handler=Mod}, ReplyTo) ->
    try
        case CallbackResult of
            {rpc_stop, Halt, NewState} ->
                apply(rpc, call,
                      [Proc#proc.id|tuple_to_list(Halt)]),
                {noreply, State#state{handler_state=NewState}};
            {stopped, NewProc, NewState} ->
                %% NB: this is an immediate stop in response to the 'stop'
                %% instruction, so *nothing* is wrong at this point
                {stop, normal, #state{proc=NewProc,
                                      handler=Mod,
                                      handler_state=NewState,
                                      activity_state=stopped}};
            {stop, Reason, NewState} ->
                {stop, Reason, State#state{handler_state=NewState}};
            {stop, Reason, NewProc, NewState} ->
                {stop, Reason, State#state{proc=NewProc,
                                           handler_state=NewState}};
            {reply, Reply, NewProc, NewState}
                    when is_record(NewProc, proc) ->
                reply(Reply, ReplyTo, State#state{proc=NewProc,
                                                  handler_state=NewState});
            {reply, Reply, NewState} ->
                reply(Reply, ReplyTo, State#state{handler_state=NewState});
            {NewProc, NewState}
                    when is_record(NewProc, proc) ->
                {noreply, State#state{proc=NewProc,
                                      handler_state=NewState}};
            NewState ->
                {noreply, State#state{handler_state=NewState}}
        end
    catch
        Ex -> {stop, Ex, State}
    end.

reply(Reply, noreply, State) ->
    %% NB: here we make sure that a handler return value (from handle_callback)
    %% which states {reply, ...} is not *incorrectly* used when the entry point
    %% to this gen_server was handle_cast or handle_info
    {stop, {handler, noreturn, Reply}, State};
reply(Reply, ReplyTo, State) ->
    gen_server:reply(ReplyTo, Reply),
    {noreply, State}.

%% proc making and configuration handling

make_proc(Config) ->
    Name = ?REQUIRE(name, Config),
    framework("make process: ~p~n", [Name]),
    %% NB: new_proc_info is an exprecs generated function
    record_fromlist([{scope,      ?REQUIRE(scope, Config)},
                     {host,       ?REQUIRE(host, Config)},
                     {name,       Name},
                     {auto_start, lookup("startup.activate_on_start",
                                         Config, true)},
                     {handler,    lookup("startup.handler",
                                         Config, systest_cli)},
                     {link,       lookup("startup.link_to_parent",
                                         Config, false)},
                     {user,       ?CONFIG(user, Config, [])},
                     {flags,      ?CONFIG(flags, Config)},
                     {apps,       ?CONFIG(applications, Config)},
                     {on_start,   ?CONFIG(on_start, Config)},
                     {on_stop,    ?CONFIG(on_stop, Config)},
                     {on_join,    ?CONFIG(on_join, Config)},
                     {cover,      lookup("startup.cover", Config, off)},
                     {config,     Config}]).

lookup(Key, Config, Default) ->
    case ?ECONFIG(Key, Config) of
        not_found -> Default;
        Value     -> Value
    end.

proc_config(Sut, Proc) ->
    Procs = systest_config:get_config(Sut, processes, []),
    framework("processes loaded from config: ~p~n", [Procs]),
    UserData = systest_config:get_config(Sut, user_data, []),
    framework("user-data loaded from config: ~p~n", [UserData]),
    %% TODO: should *this* not ?REQUIRE otherwise how do we configure the proc?
    ProcConf = case ?CONFIG(Proc, Procs, undefined) of
                   undefined               -> [];
                   Refs when is_list(Refs) -> load_config(Refs)
               end,
    UserConf = ?CONFIG(Proc, UserData, []),
    [{user, UserConf}|ProcConf].

load_config(Refs) ->
    framework("merging configuration refs: ~p~n", [Refs]),
    lists:foldl(fun merge_refs/2, [], Refs).

merge_refs(Ref, []) ->
    systest_config:require_config(Ref);
merge_refs(Ref, Acc) ->
    RefConfig = systest_config:require_config(Ref),
    Startup = systest_config:merge(
                    ?CONFIG(startup, Acc, []),
                    ?CONFIG(startup, RefConfig, [])),
    OnStart = ?CONFIG(on_start, Acc, []) ++
              ?CONFIG(on_start, RefConfig, []),
    OnStop  = ?CONFIG(on_stop, Acc, []) ++
              ?CONFIG(on_stop, RefConfig, []),
    OnJoin  = ?CONFIG(on_join, Acc, []) ++
              ?CONFIG(on_join, RefConfig, []),

    Base = [ I || I={K, _} <- Acc,
                  lists:member(K, [flags, apps, user]) ],
    [{startup, Startup},
     {on_start, OnStart},
     {on_stop, OnStop},
     {on_join, OnJoin}|Base].

setup(NI, {App, Vars}, HState) ->
    interact(NI, {applicaiton, load, [App]}, HState),
    [interact(NI, {application, set_env,
                    [App, Env, Var]}, HState) || {Env, Var} <- Vars].

