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
-module(systest_utils).

-include("systest.hrl").

-export([make_node/1, make_node/2, as_string/1, abort/2]).
-export([proplist_format/1, strip_suite_suffix/1]).
-export([proc_id_and_hostname/1, rm_rf/1, find_files/2]).
-export([with_file/3, with_termfile/2, combine/2, uniq/1]).
-export([throw_unless/2, throw_unless/3, throw_unless/4]).
-export([record_to_proplist/2, border/2, print_heading/1, print_section/2]).
-export([ets_dump/1]).

abort(Fmt, Args) ->
    io:format(user, "ERROR:  " ++ Fmt, Args),
    erlang:halt(1).

throw_unless(Cond, Msg) ->
    throw_unless(Cond, Msg, []).

throw_unless(Cond, Msg, FmtArgs) ->
    throw_unless(Cond, system, Msg, FmtArgs).

throw_unless(Cond, SubSys, Msg, FmtArgs) ->
    case Cond of
        true ->
            ok;
        false ->
            systest_log:log(SubSys, Msg, FmtArgs),
            throw({SubSys, assertion_failed})
    end.

with_termfile(Path, Handler) ->
    case file:consult(Path) of
        {ok, Terms} -> Handler(Terms);
        Error       -> Error
    end.

with_file(Path, Modes, Handler) ->
    case file:open(Path, Modes) of
        {ok, Fd} ->
            try Handler(Fd)
            after file:close(Fd)
            end;
        Error ->
            Error
    end.

uniq(List) -> sets:to_list(sets:from_list(List)).

combine(V1, V2) when is_list(V1) andalso
                     is_list(V2) -> V1 ++ V2;
combine(V1, V2) when is_list(V1) -> [V2|V1];
combine(V1, V2) when is_list(V2) -> [V1|V2];
combine(V1, V2)                  -> [V1,V2].

as_string(X) when is_atom(X)    -> atom_to_list(X);
as_string(X) when is_integer(X) -> integer_to_list(X);
as_string(X) when is_float(X)   -> float_to_list(X);
as_string(X) when is_binary(X)  -> binary_to_list(X);
as_string(X)                    -> X.

%% @doc recursive search in Dir for files matching Regex
%% @end
find_files(Dir, Rx) ->
    filelib:fold_files(Dir, Rx, true, fun(F, Acc) -> [F | Acc] end, []).

rm_rf([H|_]=Path) when is_integer(H) ->
    rm_rf([Path]);
rm_rf(Paths) when is_list(Paths) ->
    lists:foldl(fun rm_rf/2, ok, Paths).

rm_rf(_Path, {error, _}=Err) ->
    Err;
rm_rf(Path, ok) ->
    case filelib:is_dir(Path) of
        false ->
            case file:delete(Path) of
                ok              -> ok;
                {error, enoent} -> ok;
                {error, Err}    -> {error, {Path, Err}}
            end;
        true ->
            case file:list_dir(Path) of
                {ok, FileNames} -> rm_rf(FileNames);
                {error, Err}    -> {error, {Path, Err}}
            end
    end.

%% @doc make a valid erlang node shortname from Name,
%% using the current (local) hostname
%% @end
make_node(Name) ->
    {ok, Hostname} = inet:gethostname(),
    make_node(Name, list_to_atom(Hostname)).

%% @doc make a node shortname from the supplied name and host atoms
%% @end
make_node(Name, Host) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

proc_id_and_hostname(ProcId) ->
    list_to_tuple(lists:map(fun erlang:list_to_atom/1,
                            string:tokens(atom_to_list(ProcId), "@"))).

%% @doc strip the "_SUITE" suffic from a ct test suite name
%% @end
strip_suite_suffix(Suite) ->
    S = atom_to_list(Suite),
    list_to_atom(re:replace(S, "_SUITE", "", [{return, list}])).

%% @doc convert a record into a proplist using its exporting module
%% @end
record_to_proplist(Rec, Mod) ->
    [begin
        {Field, Mod:get(Field, Rec)}
     end || Field <- Mod:info(element(1, Rec))].

%% @doc convert the 'proplist' L into a printable list
%% @end
proplist_format(L) ->
    DescrLen = 1 + lists:max([length(as_string(K)) || {K, _V} <- L]),
    Padding = erlang:max(25, DescrLen),
    LenPrefix = "~-" ++ integer_to_list(Padding),
    lists:flatten(
        [begin
             Fmt = if is_list(V) andalso
                      is_integer(hd(V)) -> "~s~n";
                      true -> "~p~n"
                   end,
             io_lib:format(LenPrefix ++ "s: " ++ Fmt, [as_string(K), V])
         end || {K, V} <- L]).

print_heading(S) ->
    io:format(user, "~s~n", [border(S, "-")]).

print_section(Heading, Properties) ->
    print_heading(Heading),
    io:format(user, "~s~n", [systest_utils:proplist_format(Properties)]).

border(S, C) ->
    Pad = erlang:max(25, length(S)),
    Border = string:copies(C, Pad),
    io_lib:format("~s~n~s~n~s~n", [Border, string:centre(S, Pad), Border]).

ets_dump(Tab) ->
    print_section("Table Dump - " ++ as_string(Tab),
                  [{"Name", Tab},
                   {"Ets Info", ets:info(Tab)}|ets:tab2list(Tab)]).

