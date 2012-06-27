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
-module(systest_hooks).

-include("systest.hrl").

-export([run/3]).

-type arg()  :: {'node', atom()} |
                {'call', module(), function(), [term()]} |
                {'call_with_item', module(), function(), [term()]} |
                {'call_with_context', module(), function(), [term()]} |
                term().

-type systest_hook() :: {'eval', atom(), module(), function(), [arg()]}     |
                        {'remote', atom(), module(), function(), [arg()]}   |
                        {'local', module(), function(), [arg()]}            |
                        {module(), function(), [term()]}                    |
                        string().

%% TODO: think about the return type for this...

-spec run(Item::term(), Hook::systest_hook(), Context::term()) -> any().
run(Item, {eval, Where, Mod, Func, Args}, Context) ->
    Argv = lists:reverse(lists:foldl(fun proc_interact/2,
                                     {Item, Context, []}, Args)),
    run(Item, {Where, Mod, Func, Argv}, Context);
run(#'systest.node_info'{id=Node}, {Mod, Func, Args}, _) ->
    rpc:call(Node, Mod, Func, Args);
run(_Context, {remote, Where, Mod, Func, Args}, _) ->
    rpc:call(Where, Mod, Func, Args);
run(Context, {local, Mod, Func, Args}, _) ->
    apply(Mod, Func, [Context|Args]);
run(NI=#'systest.node_info'{handler=Handler}, Inputs, HState) ->
    Handler:handle_interaction(Inputs, NI, HState).

proc_interact({call, M, F, A}, {_Item, _Context, Acc}) ->
    [apply(M, F, A)|Acc];
proc_interact({call_with_item, M, F, A}, {Item, _Context, Acc}) ->
    [apply(M, F, [Item|A])|Acc];
proc_interact({call_with_context, M, F, A}, {Item, Context, Acc}) ->
    [apply(M, F, [Item, Context|A])|Acc];
proc_interact({node, Field}, {Item, _Context, Acc}) ->
    [systest_node:get(Field, Item)|Acc];
proc_interact(Term, {_, _, Acc}) ->
    [Term|Acc].

