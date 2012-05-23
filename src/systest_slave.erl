%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -----------------------------------------------------------------------------
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
-module(systest_slave).

-behaviour(systest_node).

-export([start/1, start_link/1, stop/1, kill/1]).
-export([status/1, interact/2]).

-include("systest.hrl").

-spec start(systest_node:node_info()) ->
                            systest_node:node_info() | term().
start(NI=#'systest.node_info'{host=Host, name=Name, vmflags=VmArgs}) ->
    on_start(NI, slave:start(Host, Name, VmArgs)).

-spec start_link(systest_node:node_info()) ->
                            systest_node:node_info() | term().
start_link(NI=#'systest.node_info'{host=Host, name=Name, vmflags=VmArgs}) ->
    on_start(NI, slave:start_link(Host, Name, VmArgs)).

-spec stop(systest_node:node_info()) -> 'ok'.
stop(#'systest.node_info'{id=Node}) ->
    slave:stop(Node).

-spec kill(systest_node:node_info()) -> 'ok'.
kill(Node) ->
    stop(Node).

-spec status(systest_node:node_info()) -> 'nodeup' | {'nodedown', term()}.
status(#'systest.node_info'{id=Node}) ->
    case net_adm:ping(Node) of
        pong  -> nodeup;
        Other -> {nodedown, Other}
    end.

-spec interact(systest_node:node_info(),
               {module(), atom(), [term()]}) -> term().
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}) ->
    rpc:call(Node, Mod, Func, Args).

%%
%% Private API
%%

on_start(NI, {ok, Node}) ->
    OsPid = rpc:call(Node, os, getpid, []),
    NI#'systest.node_info'{owner=self(), os_pid=OsPid, id=Node};
on_start(_, Error) ->
    Error.

