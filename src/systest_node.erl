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
-module(systest_node).

-include("systest.hrl").

-type node_info() :: #'systest.node_info'{}.

-export_type([node_info/0]).

-export([behaviour_info/1]).
-export([make_node/3]).
-export([interact/2]).
-export([node_id/2, node_data/1]).
-export([user_data/1, user_data/2]).
-export([start/1, start/3, stop/1, kill/1]).
-export(['kill -9'/1, stop_and_wait/1, kill_and_wait/1]).
-export([sigkill/1, kill_after/2, kill_after/3]).
-export([shutdown_and_wait/2, status/1]).
-export([joined_cluster/3]).
-export([status_check/1]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-exprecs_prefix([operation, "_"]).
-exprecs_fname([prefix, "node_info"]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records(['systest.node_info']).

-type node_ref() :: pid().
-type activity_state() :: 'running' | 'stopped' | 'killed' | 'nodedown'.

%% our own internal state is separate from that of any handler...
-record(state, {
    node            :: node_info(),
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

-spec make_node(atom(), atom(), systest_config:config()) -> node_info().
make_node(Scope, Node, Config) ->
    make_node([{ct, Config}] ++ Config ++ node_config(Scope, Node)).

-spec start(atom(), atom(),
            list(tuple(atom(), term()))) -> {'ok', pid()} | {'error', term()}.
start(Scope, Node, Config) ->
    start(make_node(Scope, Node, Config)).

-spec start(node_info()) -> {'ok', pid()} | {'error', term()}.
start(NodeInfo=#'systest.node_info'{handler=Handler, host=Host,
                                    name=Name, config=BaseConf}) ->
    ct:pal("Starting ~p on ~p~n", [Name, Host]),

    %% are there hidden traps here, when (for example) we're running
    %% embedded in an archive/escript or similarly esoteric situations?
    %% TODO: perhaps catch(Handler:id(NodeInfo)) would be safer!?
    code:ensure_loaded(Handler),

    Id = case erlang:function_exported(Handler, id, 1) of
             true  -> set_node_info([{id, Handler:id(NodeInfo)}], NodeInfo);
             false -> set_node_info([{id, node_id(Host, Name)}], NodeInfo)
         end,
    NI = set_node_info([{config,[{node, Id}|BaseConf]}], Id),

    gen_server:start_link(?MODULE, [NI], []).

-spec stop(node_ref()) -> ok.
stop(NodeRef) ->
    gen_server:cast(NodeRef, stop).

-spec kill(node_ref()) -> ok.
kill(NodeRef) ->
    gen_server:cast(NodeRef, kill).

-spec('kill -9'/1 :: (node_ref()) -> 'ok').
'kill -9'(NodeRef) ->
    sigkill(NodeRef).

-spec sigkill(node_ref()) -> 'ok'.
sigkill(NodeRef) ->
    ct:pal("[WARNING] using SIGKILL is *NOT*"
           " guaranteed to work with all node types!~n"),
    gen_server:cast(NodeRef, sigkill).

-spec kill_after(integer(), node_ref()) -> 'ok'.
kill_after(TimeoutMs, NodeRef) ->
    kill_after(TimeoutMs, NodeRef, kill).

-spec kill_after(integer(), node_ref(), atom()) -> 'ok'.
kill_after(TimeoutMs, NodeRef, Killer) ->
    {ok, _TRef} = timer:apply_after(TimeoutMs, ?MODULE, Killer, [NodeRef]),
    ok.

-spec stop_and_wait(node_ref()) -> 'ok'.
stop_and_wait(NodeRef) ->
    shutdown_and_wait(NodeRef, fun stop/1).

-spec kill_and_wait(node_ref()) -> 'ok'.
kill_and_wait(NodeRef) ->
    shutdown_and_wait(NodeRef, fun kill/1).

-spec status(node_ref()) -> 'nodeup' | {'nodedown', term()}.
status(NodeRef) ->
    safe_call(NodeRef, status, {nodedown, noproc}).

-spec interact(node_ref(), term()) -> term().
interact(NodeRef, InputData) ->
    gen_server:call(NodeRef, {interaction, InputData}).

-spec node_data(node_ref()) -> [{atom(), term()}].
node_data(NodeRef) ->
    safe_call(NodeRef, node_info_list, [{owner, NodeRef}]).

-spec user_data(node_ref()) -> [{atom(), term()}].
user_data(NodeRef) ->
    gen_server:call(NodeRef, user_data).

-spec user_data(node_ref(), term()) -> 'ok'.
user_data(NodeRef, Data) ->
    gen_server:call(NodeRef, {user_data, Data}).

-spec joined_cluster(node_ref(), pid(), [{atom(), pid()}]) -> 'ok'.
joined_cluster(NodeRef, ClusterRef, SiblingNodes) ->
    ok = gen_server:call(NodeRef, {joined, ClusterRef, SiblingNodes}).

%% NB: this *MUST* run on the client
shutdown_and_wait(Owner, ShutdownOp) when is_pid(Owner) ->
    %% TODO: review whether this makes sense in light of the changes
    %% on branch 'supervision'
    case (Owner == self()) orelse not(is_process_alive(Owner)) of
        true  -> ok;
        false -> link(Owner),
                 ct:pal("Waiting for ~p to exit from: ~p~n",
                        [Owner, erlang:process_info(self())]),
                 ok = ShutdownOp(Owner),
                 receive
                     {'EXIT', Owner, _Reason} -> ok;
                     Other                    -> ct:pal("Other ~p~n", [Other])
                 end
    end.

%%
%% Handler facing API
%%
status_check(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong  -> nodeup;
        Other -> {nodedown, Other}
    end.

%%
%% OTP gen_server API
%%

init([NodeInfo=#'systest.node_info'{handler=Callback}]) ->
    process_flag(trap_exit, true),

    case catch( apply(Callback, init, [NodeInfo]) ) of
        {ok, NI2, HState} when is_record(NI2, 'systest.node_info') ->

            %% TODO: validate that these succeed and shutdown when they don't
            case NI2#'systest.node_info'.apps of
                []   -> ok;
                Apps -> [setup(NI2, App, HState) || App <- Apps]
            end,

            NI3 = NI2#'systest.node_info'{owner=self()},
            State = #state{node=NI3, handler=Callback, handler_state=HState},

            %% TODO: validate that these succeed and shutdown when they don't
            case NI2#'systest.node_info'.on_start of
                []   -> {ok, State};
                Xtra -> {NI4, _} = lists:foldl(fun apply_startup/2,
                                               {NI3, HState}, Xtra),
                        {ok, State#state{node=NI4}}
            end;
        {error, Err} ->
            {stop, Err};
        Error ->
            %% TODO: do NOT rely on callbacks returning a proper gen_server
            %% init compatible response tuple - construct this for them....
            {stop, Error}
    end.

handle_call(Msg, From, State) ->
    handle_msg(Msg, State, From).

handle_cast(Msg, State) ->
    handle_msg(Msg, State).

handle_info(Info, State) ->
    handle_msg(Info, State).

terminate(Reason, #state{node=Node,
                         handler=Mod,
                         handler_state=ModState}) ->
    ok = Mod:terminate(Reason, Node, ModState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

safe_call(NodeRef, Msg, Default) ->
    try
        gen_server:call(NodeRef, Msg)
    catch
        _:{noproc,{gen_server,call,[NodeRef, Msg]}} ->
            Default
    end.

node_id(Host, Name) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

apply_startup(Item, NodeState) ->
    apply_hook(on_start, Item, NodeState).

apply_hook(Hook, Item, {Node, HState}) ->
    case interact(Node, Item, HState) of
        {upgrade, Node2} ->
            {Node2, HState};
        {write, Loc, Data} ->
            ct:pal("[~p] ~p~n"
                   "argv: ~p~n"
                   "state-update: ~p => ~p~n",
                   [Node#'systest.node_info'.id, Hook, Item, Loc, Data]),
            {systest_node:set_node_info([{Loc, Data}], Node), HState};
        Other ->
            ct:pal("[~p] ~p~n"
                   "argv: ~p~n"
                   "response: ~p~n",
                    [Node#'systest.node_info'.id, Hook, Item, Other]),
            {Node, HState}
    end.

on_join(Node, Cluster, Nodes, Hooks) ->
    {Node2, _} = lists:foldl(fun({M, F}, Acc) ->
                                 apply_hook(on_join,
                                            {M, F, [Node, Cluster, Nodes]},
                                            Acc);
                                ({M, F, A}, Acc) ->
                                 apply_hook(on_join,
                                            {M, F, [Node, Cluster, Nodes|A]},
                                            Acc)
                             end, {Node, undefined}, Hooks),
    Node2.

%% TODO: migrate this to systest_hooks....

interact(Node=#'systest.node_info'{id=NodeId},
         {eval, Where, Mod, Func, Args}, _HState) ->
    Argv = lists:reverse(lists:foldl(fun proc_interact/2, {Node, []}, Args)),
    case Where of
        local ->
            apply(Mod, Func, Argv);
        remote ->
            rpc:call(NodeId, Mod, Func, Argv);
        Id when is_atom(Id) ->
            rpc:call(Id, Mod, Func, Argv)
    end;
interact(Node, {local, Mod, Func, Args}, _) ->
    apply(Mod, Func, [Node|Args]);
interact(Node=#'systest.node_info'{id=Id}, {remote, Mod, Func, Args}, _) ->
    rpc:call(Id, Mod, Func, [Node|Args]);
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}, _) ->
    rpc:call(Node, Mod, Func, Args);
interact(NI=#'systest.node_info'{handler=Handler}, Inputs, HState) ->
    Handler:handle_interaction(Inputs, NI, HState).

proc_interact({plain_call, M, F, A}, {_Node, Acc}) ->
    [apply(M, F, A)|Acc];
proc_interact({call, M, F, A}, {Node, Acc}) ->
    [apply(M, F, [Node|A])|Acc];
proc_interact({node, Field}, {Node, Acc}) ->
    [get_node_info(Field, Node)|Acc];
proc_interact(Term, {_, Acc}) ->
    [Term|Acc].

handle_msg(Msg, State) ->
    handle_msg(Msg, State, noreply).

handle_msg(user_data, State=#state{node=Node}, _ReplyTo) ->
    {reply, get_node_info(user, Node), State};
handle_msg({user_data, Data}, State=#state{node=Node}, _ReplyTo) ->
    {reply, 'ok', State#state{node=set_node_info([{user, Data}], Node)}};
handle_msg(node_info_list, State=#state{node=Node}, _ReplyTo) ->
    Attrs = systest_node:info_node_info(fields) -- [config],
    Info = [{K, get_node_info(K, Node)} || K <- Attrs],
    {reply, Info, State};
handle_msg({joined, Cluster, Nodes}, State=#state{node=Node}, _ReplyTo) ->
    case Node#'systest.node_info'.on_join of
        []    -> {reply, ok, State};
        Hooks -> Node2 = on_join(Node, Cluster, Nodes, Hooks),
                 {reply, ok, State#state{node=Node2}}
    end;
%% nodedown notifications
handle_msg({nodedown, NodeId},
           SvrState=#state{activity_state=State,
                           node=#'systest.node_info'{id=NodeId}},
           _ReplyTo) ->
    ShutdownType = case State of
                       killed  -> normal;
                       stopped -> normal;
                       _       -> nodedown
                   end,
    {stop, ShutdownType, SvrState};
handle_msg({nodedown, NodeId},
           State=#state{activity_state=running,
                        node=#'systest.node_info'{id=NodeId}}, _ReplyTo) ->
    {stop, nodedown, State#state{activity_state=nodedown}};
%% instructions from clients
handle_msg(stop, State=#state{activity_state=stopped}, _) ->
    {noreply, State};
handle_msg(kill, State=#state{activity_state=stopped}, _) ->
    {noreply, State};
handle_msg(stop, State=#state{activity_state=killed}, _) ->
    {noreply, State};
%% TODO: consider whether this should be disallowed, or ignored
% handle_msg(kill, State=#state{activity_state=stopped}, _) ->
%    {noreply, State};
handle_msg(stop, State=#state{node=Node, handler=Mod,
                              handler_state=ModState}, ReplyTo) ->
    case Node#'systest.node_info'.on_stop of
        [] -> ok;
        %% TODO: consider whether this is structured correctly - it *feels*
        %% a little hackish - and perhaps having a supervising process deal
        %% with these 'interactions' would be better
        Shutdown  -> [ct:pal("~p~n",
                        [interact(Node, In, ModState)]) || In <- Shutdown]
    end,
    handle_callback(stopping_callback(Mod:handle_stop(Node, ModState)),
                    State#state{activity_state=stopped}, ReplyTo);
handle_msg(kill, State=#state{node=Node, handler=Mod,
                              handler_state=ModState}, ReplyTo) ->
    handle_callback(stopping_callback(Mod:handle_kill(Node, ModState)),
                    State#state{activity_state=killed}, ReplyTo);
handle_msg(sigkill, State=#state{node=Node, handler=Mod,
                                 handler_state=ModState}, ReplyTo) ->
    handle_callback(stopping_callback(Mod:handle_msg(sigkill, Node, ModState)),
                    State#state{activity_state=killed}, ReplyTo);
handle_msg(status, State=#state{activity_state=stopped}, _ReplyTo) ->
    {reply, {stopping, stopped}, State};
handle_msg(status, State=#state{activity_state=killed}, _ReplyTo) ->
    {reply, {stopped, killed}, State};
handle_msg(status, State=#state{node=Node, handler=Mod,
                                handler_state=ModState}, ReplyTo) ->
    handle_callback(Mod:handle_status(Node, ModState), State, ReplyTo);
handle_msg({interaction, _},
           State=#state{activity_state=ActivityState}, _)
                when ActivityState =:= killed orelse
                     ActivityState =:= stopped ->
    {reply, {error, stopping}, State};
handle_msg({interaction, InputData},
            State=#state{node=Node, handler=Mod,
                         handler_state=ModState}, ReplyTo) ->
    ct:pal("handle_interaction: ~p~n", [InputData]),
    handle_callback(Mod:handle_interaction(InputData,
                                           Node, ModState), State, ReplyTo);
%% our catch-all, which defers to Mod:handler_state/3 to see if the
%% callback module knows what to do with Msg or not - this also allows the
%% handler an opportunity to decide how to deal with unexpected messages
handle_msg(Msg, State=#state{node=Node, handler=Mod,
                             handler_state=ModState}, ReplyTo) ->
    handle_callback(Mod:handle_msg(Msg, Node, ModState), State, ReplyTo).

stopping_callback(Result) ->
    case Result of
        {stop, NewNode, NewModState} ->
            {stopped, NewNode, NewModState};
        Other ->
            Other
    end.

handle_callback(CallbackResult,
                State=#state{node=Node,
                             handler=Mod}, ReplyTo) ->
    try
        case CallbackResult of
            {rpc_stop, Halt, NewState} ->
                apply(rpc, call,
                      [Node#'systest.node_info'.id|tuple_to_list(Halt)]),
                {noreply, State#state{handler_state=NewState}};
            {stopped, NewNode, NewState} ->
                %% NB: this is an immediate stop in response to the 'stop'
                %% instruction, so *nothing* is wrong at this point
                {stop, normal, #state{node=NewNode,
                                      handler=Mod,
                                      handler_state=NewState,
                                      activity_state=stopped}};
            {stop, Reason, NewState} ->
                {stop, Reason, State#state{handler_state=NewState}};
            {stop, Reason, NewNode, NewState} ->
                {stop, Reason, State#state{node=NewNode,
                                           handler_state=NewState}};
            {reply, Reply, NewNode, NewState}
                    when is_record(NewNode, 'systest.node_info') ->
                reply(Reply, ReplyTo, State#state{node=NewNode,
                                                  handler_state=NewState});
            {reply, Reply, NewState} ->
                reply(Reply, ReplyTo, State#state{handler_state=NewState});
            {NewNode, NewState}
                    when is_record(NewNode, 'systest.node_info') ->
                {noreply, State#state{node=NewNode,
                                      handler_state=NewState}};
            NewState ->
                {noreply, State#state{handler_state=NewState}}
        end
    catch
        Ex -> {stop, Ex, State}
    end.

reply(Reply, noreply, State) ->
    %% NB: here we make sute that a handler return value (from handle_callback)
    %% which states {reply, ...} is not *incorrectly* used when the entry point
    %% to this gen_server was handle_cast or handle_info
    {stop, {handler, noreturn, Reply}, State};
reply(Reply, ReplyTo, State) ->
    gen_server:reply(ReplyTo, Reply),
    {noreply, State}.

%% node making and configuration handling

make_node(Config) ->
    %% NB: new_node_info is an exprecs generated function
    new_node_info([{scope,      ?REQUIRE(scope, Config)},
                   {host,       ?REQUIRE(host, Config)},
                   {name,       ?REQUIRE(name, Config)},
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
                   {config,     Config}]).

lookup(Key, Config, Default) ->
    case ?ECONFIG(Key, Config) of
        not_found -> Default;
        Value     -> Value
    end.

node_config(Cluster, Node) ->
    Nodes = systest_config:get_config({Cluster, nodes}),
    UserData = systest_config:get_config({Cluster, user_data}),
    ct:pal("Checking for ~p in ~p of ~p~n", [Node, Cluster, Nodes]),
    NodeConf = case ?CONFIG(Node, Nodes, undefined) of
                   undefined               -> [];
                   Refs when is_list(Refs) -> load_config(Refs)
               end,
    [{user, ?CONFIG(Node, UserData, [])}|NodeConf].

load_config(Refs) ->
    lists:foldl(fun merge_refs/2, [], Refs).

merge_refs(Ref, []) ->
    systest_config:get_config(Ref);
merge_refs(Ref, Acc) ->
    RefConfig = systest_config:get_config(Ref),
    Startup = systest_config:merge_config(
                    ?CONFIG(startup, Acc, []),
                    ?CONFIG(startup, RefConfig, [])),
    OnStart = ?CONFIG(on_start, Acc, []) ++
              ?CONFIG(on_start, RefConfig, []),
    OnStop  = ?CONFIG(on_stop, Acc, []) ++
              ?CONFIG(on_stop, RefConfig, []),

    Base = [ I || I={K, _} <- Acc,
                  lists:member(K, [on_join, flags, apps, user]) ],
    [{startup, Startup},
     {on_start, OnStart},
     {on_stop, OnStop}|Base].

setup(NI, {App, Vars}, HState) ->
    interact(NI, {applicaiton, load, [App]}, HState),
    [interact(NI, {application, set_env,
                    [App, Env, Var]}, HState) || {Env, Var} <- Vars].

