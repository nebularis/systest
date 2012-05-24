%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
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
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(systest_node).

-include("systest.hrl").

-type node_info() :: #'systest.node_info'{}.

-export_type([node_info/0]).

-export([behaviour_info/1]).
-export([make_node/3]).
-export([start/1, stop/1, kill/1]).
-export([status/1, interact/2]).

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
start(NodeInfo=#'systest.node_info'{handler=Handler, link=ShouldLink}) ->
    Startup = case ShouldLink of true -> start_link; _ -> start end,
    ct:pal("~p starting node: ~p~n", [Handler, NodeInfo]),
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

-spec status(node_info()) -> 'nodeup' | {'nodedown', term()}.
status(NI=#'systest.node_info'{handler=Handler}) ->
    Handler:status(NI).

-spec interact(node_info(), term()) -> term().
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}) ->
    rpc:call(Node, Mod, Func, Args);
interact(NI=#'systest.node_info'{handler=Handler}, Inputs) ->
    Handler:interact(NI, Inputs).

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
                   {link, ?CONFIG(link, Config, false)},
                   {user, ?CONFIG(user, Config, os:getenv("USER"))},
                   {flags, ?CONFIG(flags, Config)},
                   {apps, ?CONFIG(applications, Config)},
                   {extra, ?CONFIG(extra, Config)},
                   {config, Config}]).

node_config(Cluster, Node, Config) ->
    Globals         = systest_config:get_config(global_node_config),
    NodeConfig      = systest_config:get_config({Cluster, Node}),
    {Static, Runtime, Flags} = lists:foldl(fun extract_config/2,
                                           {[], [], []}, NodeConfig),

    RuntimeConfig1  = systest_config:merge_config(Config, NodeConfig),
    RuntimeConfig2  = systest_config:merge_config(RuntimeConfig1, Runtime),
    Config2         = systest_config:merge_config(Static, RuntimeConfig2),
    MergedGlobals1  = systest_config:merge_config(Globals, Config2),
    MergedFlags     = systest_config:merge_config(MergedGlobals1, Flags),
    MergedFlags.

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
