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
-module(systest_config).
-include_lib("eunit/include/eunit.hrl").

-include("systest.hrl").

-type config_key() :: atom() | string() | binary().
-type config()     :: [{config_key(), term()}].

-export_type([config_key/0, config/0]).

-export([read/2, read/3, require/2]).
-export([eval/2, eval/3, sut_config/2]).
-export([replace_value/3, ensure_value/3]).
-export([get_config/1, get_config/2, get_config/3, merge/2]).
-export([get_env/0, get_env/1, set_env/2]).
-export([load_config/2, load_config_terms/1, load_config_terms/2]).
-export([get_static/1, set_static/2]).
-export([start_link/0, dump/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(systest_utils, [as_string/1]).

-type key_type()    :: 'atom' | 'binary' | 'string'.
-type return_spec() :: 'value' | 'key' | 'path'.

-record(search, {
    key_type   :: key_type(),
    key_index  :: integer(),
    key_func   :: fun((term()) -> term()),
    return     :: return_spec(),
    callbacks  :: [{atom(), fun((term(), term()) -> term())}], %% too loose!
    source     :: [{term(), term()}]
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sut_config(Scope, Identity) ->
    case is_configured_explicitly(Identity) of
        {true, Config} ->
            {Identity, Config};
        false ->
            case search({Identity, 'all'},
                        get_config(Scope, []),
                        search_options([{key_func, fun(X) -> X end}])) of
                Bad when Bad =:= not_found orelse
                         Bad =:= undefined ->
                    systest_log:log(framework,
                                    "nothing at ~p.(~p|all)~n",
                                    [Scope, Identity]),
                    {Identity, extract_sut_config(Identity)};
                Alias when is_atom(Alias) ->
                    {Alias, extract_sut_config(Alias)};
                Other ->
                    throw(Other)
            end
    end.

%% @private
is_configured_explicitly(Identity) ->
    case get_config(Identity, sut, noconfig) of
        noconfig ->
            false;
        Cfg ->
            {true, Cfg ++ [{on_start, get_config(Identity, on_start, [])}]}
    end.

extract_sut_config(Identity) ->
    case get_config(Identity, sut, noconfig) of
        noconfig ->
            noconfig;
        Cfg ->
            Cfg ++ [{on_start, get_config(Identity, on_start, [])}]
    end.

eval(Key, Config) ->
    eval(Key, Config, []).

eval(Path, Config, Opts) when is_list(Opts) ->
    eval(Path, Config, search_options(Opts));
eval(Path, Config, Search=#search{return=Spec, key_func=KF}) ->
    % io:format(user, "eval ~s~n", [Path]),
    try
        Parts = string:tokens(Path, "."),
        Result = search_eval(Parts, Config, Search),
        case param_eval(Result, Config, Search) of
            not_found -> not_found;
            Result2   -> case Spec of
                             key   -> Key = hd(lists:reverse(Parts)),
                                      {KF(Key), Result2};
                             path  -> {Path, Result2};
                             value -> Result2
                         end
        end
    catch
        _:not_found -> not_found
    end.

param_eval({Tag, Value}, Config, Opts) ->
    {Tag, param_eval(Value, Config, Opts)};
param_eval([H|_]=String, Config, Opts) when is_integer(H) ->
    Groups = re:split(String, "(\\$|%)\\{([^\\}]*)(?:\\}|%)",
                      [{return, list}, group, trim]),
    string:join([string_eval(P, Config, Opts) || P <- Groups], "");
param_eval(Values, Config, Opts) when is_list(Values) ->
    [param_eval(V, Config, Opts) || V <- Values];
param_eval(Other, _, _) ->
    Other.

string_eval(Parts, Config, Opts) ->
    lists:flatten(lists:reverse(
        lists:foldl(
            fun(E, ["$"|Acc]) -> [as_string(eval(E, Config, Opts))|Acc];
               (E, ["%"|Acc]) -> [require_env(E)|Acc];
               (E, Acc)       -> [E|Acc]
            end, [], Parts))).

search_eval(_, not_found, _) ->
    not_found;
search_eval([], Result, #search{return=value}) ->
    Result;
search_eval([], Result, _) ->
    Result;
search_eval([Current|Remaining], {callback, Config, Fun},
                                 Opts=#search{key_func=KF}) ->
    search_eval(Remaining, Fun(KF(Current), Config), Opts);
search_eval([Current|Remaining], {callback, Config, Fun}, Opts) ->
    search_eval(Remaining, Fun(Current, Config), Opts);
search_eval([Current|Remaining], Config, Opts=#search{key_func='id'}) ->
    search_eval(Remaining, search(Current, Config, Opts), Opts);
search_eval([Current|Remaining], Config, Opts=#search{key_func=KF}) ->
    search_eval(Remaining, search(KF(Current), Config, Opts), Opts).

search({Either, Or}, Config, Search) ->
    case search(Either, Config, Search) of
        not_found ->
            search(Or, Config, Search);
        Thing ->
            Thing
    end;
search(Key, Config, Search=#search{callbacks=CB}) ->
    case lists:keyfind(Key, 1, CB) of
        {Key, Fun} -> {callback, key_search(Key, Config, Search), Fun};
        false      -> key_search(Key, Config, Search)
    end.

key_search(Key, Config, #search{key_index=Kidx}) ->
    case lists:keyfind(Key, Kidx, Config) of
        false                    -> not_found;
        {Key, Value}             -> Value;
        Value
            when is_tuple(Value) -> Value
    end.

search_options(Opts) ->
    #search{key_type  = ?CONFIG(key_type,  Opts, 'atom'),
            key_index = ?CONFIG(key_index, Opts, 1),
            key_func  = ?CONFIG(key_func,  Opts, fun erlang:list_to_atom/1),
            return    = ?CONFIG(return,    Opts, 'value'),
            callbacks = proplists:get_all_values(callback, Opts),
            source    = Opts}.

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

get_env() ->
    gen_server:call(?MODULE, list).

get_env(Key) ->
    lookup(Key).

require_env(Key) ->
    case lookup(Key) of
        {Key, Value} -> Value;
        _            -> throw(not_found)
    end.

set_env(Key, Value) ->
    ok = gen_server:call(?MODULE, {set, Key, Value}).

load_config(Id, Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            load_config_terms(Id, Terms);
        Error ->
            Error
    end.

load_config_terms(Id, Terms) ->
    gen_server:call(?MODULE, {load, Id, Terms}).

load_config_terms(Terms) ->
    gen_server:call(?MODULE, {load, Terms}).

dump() ->
    gen_server:call(?MODULE, dump).

get_config(Key) ->
    systest_log:log(framework,
                    "reading config key ~p~n", [Key]),
    case gen_server:call(?MODULE, {get_config, Key}) of
        []     -> noconfig;
        Config -> Config
    end.

get_config(Key, Default) ->
    case get_config(Key) of
        noconfig -> Default;
        Config   -> Config
    end.

get_config(Key, Node, Default) ->
    systest_log:log(framework,
                    "reading config key ~p.~p~n", [Key, Node]),
    case gen_server:call(?MODULE, {get_config, Key, Node}) of
        noconfig -> noconfig;
        Nodes    -> read(Node, Nodes, Default)
    end.

get_static(Key) ->
    systest_log:log(framework,
                    "reading config key ~p~n", [Key]),
    case gen_server:call(?MODULE, {get_static, Key}) of
        [Config] -> Config;
        []              -> noconfig;
        Other           -> Other
    end.

set_static(Key, Value) ->
    gen_server:call(?MODULE, {set_static, Key, Value}).

merge([], C2) ->
    C2;
merge(C1, []) ->
    C1;
merge(Config1, Config2) ->
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
    ets:new(systest_static_cs, [ordered_set, named_table, protected,{keypos,1},
                        {write_concurrency, false}, {read_concurrency, true}]),
    {ok, []}.

handle_call({set, Key, Value}, _From, State) ->
    true = ets:insert(?MODULE, [{Key, Value}]),
    {reply, ok, State};
handle_call(list, _From, State) ->
    {reply, ets:tab2list(?MODULE), State};
handle_call({load, Id, Terms}, _From, State) ->
    case ets:info(Id) of
        undefined -> ets:new(Id, [bag, named_table,
                                  protected, {keypos, 1},
                                  {write_concurrency, false},
                                  {read_concurrency, true}]);
        _T        -> ok
    end,
    true = ets:insert(Id, Terms),
    {reply, ok, State};
handle_call({load, Terms}, _From, State) ->
    Ids = [
    begin
        case ets:info(Id) of
            undefined -> ets:new(Id, [set, named_table,
                                      protected, {keypos, 1},
                                      {write_concurrency, false},
                                      {read_concurrency, true}]);
            _         -> ok
        end,
        true = ets:insert(Id, Config),
        Id
    end || {Id, Config} <- Terms],
    {reply, ok, Ids ++ State};
handle_call({get_static, Key}, From, State) ->
    handle_call({get_config, systest_static_cs, Key}, From, State);
handle_call({set_static, Key, Value}, _From, State) ->
    case catch(ets:insert(systest_static_cs, {Key, Value})) of
        {'EXIT', Reason} -> {reply, {error, Reason}, State};
        true             -> {reply, ok, State}
    end;
handle_call({get_config, Key}, _From, State) ->
    %% TODO: now that we hold dynamic tables in State,
    %% we *could* use lists:member(Key, State) instead...
    case catch(ets:tab2list(Key)) of
        {'EXIT', Reason} -> {reply, noconfig, State};
        Result           -> {reply, Result, State}
    end;
handle_call({get_config, Key, SubKey}, _From, State) ->
    %% TODO: now that we hold dynamic tables in State,
    %% we *could* use lists:member(Key, State) instead...
    case catch(ets:lookup(Key, SubKey)) of
        {'EXIT', Reason} -> {reply, noconfig, State};
        Result           -> {reply, Result, State}
    end;
handle_call(dump, _From, State) ->
    [systest_utils:ets_dump(Tab) || Tab <- [?MODULE,
                                            systest_static_cs] ++ State],
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
%% Private/Internal API
%%

merge2(New, Existing) when is_tuple(New) ->
    K = element(1, New),
    case lists:keymember(K, 1, Existing) of
        true ->
            lists:keyreplace(K, 1, Existing, New);
        false ->
            [New|Existing]
    end;
merge2(New, Existing) ->
    append_if_missing(fun lists:member/2, New, Existing).

%% TODO: this head (clause) isn't used any more!?
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
            merge2(New, Existing);
        true  ->
            ScratchDir = proplists:get_value(priv_dir, Existing),
            {Value, _} = lists:foldl(fun rewrite_path_spec/2,
                                     {[], ScratchDir}, Members),
            [{Path, Rewrite(Value)}|Existing]
    end;
%% TODO: deprecate 'extra' as it is no longer used
extend({extra, [{_, _, _}|_]=Stuff}, Existing) ->
    Items = [{M,F,[hd(extend(A, Existing)) || A <- Args]} ||
                                                        {M,F,Args} <- Stuff],
    [{extra, Items}|Existing];
extend({K, NewVal}=New, Existing) when is_list(NewVal) ->
    case lists:keyfind(K, 1, Existing) of
        {K, [{_,_}|_]=OldVal} ->
            Extender = case K of
                           Action when Action =:= start orelse
                                       Action =:= stop  orelse
                                       Action =:= kill  orelse
                                       Action =:= status -> fun merge2/2;
                           _ -> fun extend/2
                       end,
            NewEntry = {K, lists:foldl(Extender, OldVal, NewVal)},
            lists:keyreplace(K, 1, Existing, NewEntry);
        _ ->
            lists:keyreplace(K, 1, Existing, New)
    end;
%% TODO: deprecate 'environment' tuples, as they're not longer used
extend({environment, _}=New, Existing) ->
    [New|Existing];
extend({environment, _, _}=New, Existing) ->
    [New|Existing];
extend({_, _}=New, Existing) ->
    merge2(New, Existing);
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
    [A, B] = re:split(Var, "=", [{return,list}, {parts,2}]),
    [{list_to_atom(string:to_lower(A)), B}, {A, B}].
