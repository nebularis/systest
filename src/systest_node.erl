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
-export([start/1, stop/1, kill/1]).
-export([sigkill/1, stop_and_wait/1, kill_and_wait/1]).
-export([shutdown_and_wait/2, status/1, interact/2]).

-export([status_check/1]).

-exprecs_prefix([operation, "_"]).
-exprecs_fname([prefix, "node_info"]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records(['systest.node_info']).

%%
%% Public API
%%

-spec behaviour_info(term()) -> any().
behaviour_info(callbacks) ->
    [{start,        1},
     {start_link,   1},
     {stop,         1},
     {kill,         1},
     {status,       1},
     {interact,     2}];
behaviour_info(_) ->
    undefined.

-spec make_node(atom(), atom(), systest_config:config()) -> node_info().
make_node(Cluster, Node, Config) ->
    make_node(node_config(Cluster, Node, Config)).

-spec start(node_info()) -> node_info() | term().
start(NodeInfo=#'systest.node_info'{handler=Handler, link=ShouldLink,
                                    host=Host, name=Name}) ->
    ct:pal("Starting ~p on ~p~n", [Name, Host]),
    Startup = case ShouldLink of true -> start_link; _ -> start end,
    case catch( apply(Handler, Startup, [NodeInfo]) ) of
        NI2 when is_record(NI2, 'systest.node_info') ->
            case NI2#'systest.node_info'.apps of
                []   -> ok;
                Apps -> [setup(NI2, App) || App <- Apps]
            end,
            %% TODO: should we validate that these succeed?
            case NI2#'systest.node_info'.extra of
                []   -> ok;
                Xtra -> [ct:pal("~p", [interact(NI2, In)]) || In <- Xtra]
            end,
            {ok, NI2};
        Error ->
            throw(Error)
    end.

-spec stop(node_info()) -> ok.
stop(NI=#'systest.node_info'{handler=Handler}) ->
    Handler:stop(NI).

-spec kill(node_info()) -> ok.
kill(NI=#'systest.node_info'{handler=Handler}) ->
    Handler:kill(NI).

-spec sigkill(node_info()) -> string().
sigkill(#'systest.node_info'{os_pid=Pid}) ->
    ct:log("executing kill -9 ~s~n", [Pid]),
    Result = os:cmd("kill -9 " ++ Pid),
    ct:log(Result).

-spec stop_and_wait(node_info()) -> 'ok'.
stop_and_wait(NI) when is_record(NI, 'systest.node_info') ->
    shutdown_and_wait(NI, fun stop/1).

-spec kill_and_wait(node_info()) -> 'ok'.
kill_and_wait(NI) when is_record(NI, 'systest.node_info') ->
    shutdown_and_wait(NI, fun kill/1).

-spec status(node_info()) -> 'nodeup' | {'nodedown', term()}.
status(NI=#'systest.node_info'{handler=Handler}) ->
    Handler:status(NI).

-spec interact(node_info(), term()) -> term().
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}) ->
    rpc:call(Node, Mod, Func, Args);
interact(NI=#'systest.node_info'{handler=Handler}, Inputs) ->
    Handler:interact(NI, Inputs).

shutdown_and_wait(NI=#'systest.node_info'{owner=Owner},
                  ShutdownOp) when is_pid(Owner) ->
    case (Owner == self()) orelse not(is_process_alive(Owner)) of
        true  -> ok;
        false -> link(Owner),
                 ct:log("Stopping ~p ....~n"
                        "Waiting for port owning process (~p) to exit...~n",
                        [NI#'systest.node_info'.id, Owner]),
                 ok = ShutdownOp(NI),
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
%% Private API
%%

make_node(Config) ->
    %% NB: new_node_info is an exprecs generated function
    new_node_info([{host, ?REQUIRE(host, Config)},
                   {name, ?REQUIRE(name, Config)},
                   {handler, ?CONFIG(handler, Config, systest_cli)},
                   {link, ?CONFIG(link_to_parent,
                                    ?CONFIG(startup, Config, []), false)},
                   {user, ?CONFIG(user, Config, os:getenv("USER"))},
                   {flags, ?CONFIG(flags, Config)},
                   {apps, ?CONFIG(applications, Config)},
                   {extra, ?CONFIG(extra, Config)},
                   {config, Config}]).

node_config(Cluster, Node, Config) ->
    %% TODO: this code does *NOT* work for all possible merge scenarios!
    [AllGlobals]    = systest_config:get_config(global_node_config),
    ct:pal("Globals: ~p~n", [AllGlobals]),
    [ClusterConfig] = systest_config:get_config(Cluster),
    ct:pal("ClusterConfig: ~p~n", [ClusterConfig]),

    Globals         = systest_config:merge_config(AllGlobals, ClusterConfig),
    [NodeConfig]    = systest_config:get_config({Cluster, Node}),

    ct:pal("Globals: ~p~n", [Globals]),
    ct:pal("NodeConfig: ~p~n", [NodeConfig]),
    
    {Static, Runtime, Flags} = lists:foldl(fun extract_config/2,
                                           {[], [], []}, NodeConfig),

    ct:pal("Flags: ~p~n", [Flags]),

    GlobalFlags = ?CONFIG(flags, Globals),
    FlagsConfig = [begin
                       GF = ?CONFIG(Key, GlobalFlags),
                       {Key, systest_config:merge_config(GF, NF)}
                   end || {Key, NF} <- Flags],

    ct:pal("FlagsConfig: ~p~n", [FlagsConfig]),

    %% FlagsConfig = systest_config:merge_config(Flags, GlobalFlags),
    Config2 = systest_config:merge_config(Static, Runtime),
    MergedConfig = systest_config:merge_config(Globals, Config2),
    Config3 = systest_config:merge_config(MergedConfig, Config),
    
    Config4 = lists:keyreplace(flags, 1, Config3, {flags, FlagsConfig}),
    ct:pal("Config4: ~p~n", [Config4]),
    Config4.

extract_config({static, Static}, {SoFar, _, _}=Acc) ->
    setelement(1, Acc, Static ++ SoFar);
extract_config({startup, Startup}, {SoFar, _, _}=Acc) ->
    setelement(1, Acc, Startup ++ SoFar);
extract_config({runtime, Runtime}, {_, SoFar, _}=Acc) ->
    setelement(2, Acc, Runtime ++ SoFar);
extract_config({flags, Flags}, {_, _, SoFar}=Acc) ->
    setelement(3, Acc, Flags ++ SoFar).

setup(NI, {App, Vars}) ->
    interact(NI, {applicaiton, load, [App]}),
    [interact(NI, {application, set_env, [App, Env, Var]}) ||
                                            {Env, Var} <- Vars].
