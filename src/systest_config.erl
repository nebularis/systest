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

-type config_key() :: atom() | string() | binary(). 
-type config()     :: [{config_key(), term()}].

-export_type([config_key/0, config/0]).

-export([read/2, read/3, require/2]).
-export([replace_value/3, ensure_value/3]).
-export([get_config/1, get_config/3, merge_config/2]).
-export([get_env/1, set_env/2]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%
%% Public API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read(Key, Config, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false        -> Default;
        {Key, Value} -> Value
    end.

read(Key, Config) ->
    read(Key, Config, []).

require(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            ct:fail("Required config element ~p not found!~n", [Key]);
        {Key, Value} ->
            Value
    end.

%% TODO: move these API calls into the gen_server mechanism, so that we can 
%% easily switch between common_test and stand-alone runs if we wish...

get_env(Key) ->
    lookup(Key).

set_env(Key, Value) ->
    ok = gen_server:call(?MODULE, {set, Key, Value}).

get_config(Key) ->
    ct:get_config(Key, [all], []).

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
%% OTP gen_server API
%%

init([]) ->
    catch ets:delete(?MODULE),
    %% TODO: we can make this protected instead...
    ets:new(?MODULE, [ordered_set, named_table, protected, {keypos,1},
                      {write_concurrency, false}, {read_concurrency, true}]),
    ets:insert_new(?MODULE,
                   lists:flatten([to_tuple(Var) || Var <- os:getenv()])),
    {ok, []}.

handle_call({set, Key, Value}, _From, State) ->
    true = ets:insert([{Key, Value}]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    merge(New, Existing);
extend(Other, _Existing) ->
    Other.

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

lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
        []       -> not_found;
        [Config] -> Config
    end.

to_tuple(Var) ->
    [A, B] = re:split(Var, "=", [{return,list},{parts,2}]),
    [{list_to_atom(string:to_lower(A)), B}, {A, B}].
