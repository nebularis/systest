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
-module(systest_config).
-include_lib("eunit/include/eunit.hrl").

-export([read/2, require/2]).
-export([replace_value/3, ensure_value/3]).
-export([get_config/3, merge_config/2]).

%%
%% Public API
%%

read(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false        -> [];
        {Key, Value} -> Value
    end.

require(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            ct:fail("Required config element ~p not found!~n", [Key]);
        {Key, Value} ->
            Value
    end.

get_config(Scope, Node, Type) ->
    Nodes = ct:get_config({Scope, Node}, [all], []),
    read(Type, Nodes).

merge_config(Config1, Config2) ->
    lists:foldl(fun extend/2, Config1, Config2).

ensure_value(Key, Value, PList) ->
    case lists:keyfind(Key, 1, PList) of
        false ->
            [{Key, Value}|PList];
        {Key, ExistingValues} ->
            lists:keyreplace(Key, 1, PList,
                             {Key, [Value|ExistingValues]})
    end.

replace_value(Key, Value, PList) ->
    case lists:keymember(Key, 1, PList) of
        true  -> lists:keyreplace(Key, 1, PList, {Key, Value});
        false -> [{Key, Value}|PList]
    end.

%%
%% Private API
%%

merge(New, Existing) when is_tuple(New) ->
    K = element(1, New),
    case lists:keymember(K, 1, Existing) of
        true ->
            lists:keyreplace(K, 1, Existing, New);
        false ->
            [New|Existing]
    end;
merge(New, Existing) ->
    append_if_missing(fun lists:member/2, New, Existing).

extend({Path, Spec}=New, Existing) when Path =:= dir orelse
                                        Path =:= file ->
    %% TODO: this isn't very flexible - make it simpler to use...
    {Rewrite, Members} = case Spec of
                             {path, Values} ->
                                 {fun filename:join/1, Values};
                             Parts when is_list(Parts) ->
                                 {fun lists:flatten/1, Parts}
                         end,
    case lists:member(scratch, Members) of
        false ->
            merge(New, Existing);
        true  ->
            ScratchDir = proplists:get_value(priv_dir, Existing),
            {Value, _} = lists:foldl(fun rewrite_path_spec/2,
                                     {[], ScratchDir}, Members),
            [{Path, Rewrite(Value)}|Existing]
    end;
extend({extra, [{_, _, _}|_]=Stuff}, Existing) ->
    Items = [{M,F,[hd(extend(A, Existing)) || A <- Args]} || {M,F,Args} <- Stuff],
    [{extra, Items}|Existing];
extend({K, NewVal}=New, Existing) when is_list(NewVal) ->
    case lists:keyfind(K, 1, Existing) of
        {K, OldVal} when is_list(OldVal) ->
            NewEntry = {K, lists:foldl(fun extend/2, OldVal, NewVal)},
            lists:keyreplace(K, 1, Existing, NewEntry);
        false ->
            [New|Existing];
        Other ->
            ct:fail("Cannot merge incoming config ~p with ~p~n", [New, Other])
    end;
extend({_, _}=New, Existing) ->
    merge(New, Existing).
    %append_if_missing(fun({K, _}, Items) -> lists:keymember(K, 1, Items) end,
    %                  New, Existing);

append_if_missing(LookupFun, New, Existing) ->
    case LookupFun(New, Existing) of
        true ->
            Existing;
        false ->
            [New|Existing]
    end.

rewrite_path_spec(scratch, {Acc, Dir}) ->
    {Acc ++ [Dir], Dir};
rewrite_path_spec(Other, {Acc, Dir}) ->
    {Acc ++ [Other], Dir}.


