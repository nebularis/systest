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
-module(systest_cover).

-include("systest.hrl").

-export([start/2, start_cover/1, stop_cover/1, report_cover/3]).

-spec start(file:filename(),
            systest_config:config()) -> {'ok', file:filename()} |
                                        {error, term()}.
start(ScratchDir, Config) ->
    case ?CONFIG(dryrun, Config, ?CONFIG(no_cover, Config, false)) of
        true  -> {ok, dryrun};
        false -> do_start(ScratchDir, Config)
    end.

start_cover(Node) ->
    do_if_cover_enabled(fun(N) ->
                            systest_log:log({framework, Node},
                                    "starting cover on remote node~n", []),
                            cover:start(N)
                        end, Node).

stop_cover(Node) ->
    do_if_cover_enabled(fun(N) ->
                            systest_log:log({framework, Node},
                                    "stopping cover on remote node~n", []),
                            cover:stop(N)
                        end, Node).

do_if_cover_enabled(Fun, Node) ->
    case whereis(cover_server) of
        undefined -> ok;
        _         -> Fun(Node)
    end.

do_start(ScratchDir, Config) ->
    cover:start(),

    CoverBase = filename:join(ScratchDir, "cover"),
    CoverData = filename:join(CoverBase, "data"),
    ImportData = case ?CONFIG('cover-import', Config, false) of
                     false ->
                         [];
                     true ->
                         File = filename:join(CoverData, "cover.export"),
                         case filelib:is_regular(File) of
                             true  -> [File];
                             false -> []
                         end
                 end,

    CoverImports = ImportData ++ ?CONFIG('cover-import-extra', Config, []),

    SearchDirs = proplists:get_all_values('cover-dir', Config),
    {ok, Cwd} = file:get_cwd(),
    Dirs = [case filename:pathtype(Dir) of
                relative -> filename:join(Cwd, Dir);
                absolute -> Dir
            end || Dir <- SearchDirs],

    [begin
         systest_log:log("cover compiling modules in ~s~n", [D]),
         Results = cover:compile_beam_directory(D),
         case [R || R <- Results, element(1, R) =:= error] of
             []     -> ok;
             Errors -> error(Errors)
         end
     end || D <- Dirs],

    [begin
         systest_log:log(framework, "importing cover data from ~s~n", [F]),
         ok = cover:import(F)
     end || F <- CoverImports],

    CoverFile = "systest-" ++ systest_env:timestamp() ++ ".cover",
    Export = filename:join(CoverData, CoverFile),
    filelib:ensure_dir(Export),
    {ok, Export}.

-spec report_cover(file:filename(),
                   file:filename(),
                   systest_config:config()) -> ok.
report_cover(_Dir, dryrun, _Config) ->
    ok;
report_cover(BaseDir, Export, Config) ->
    io:nl(),
    systest_utils:print_heading("Building Code Coverage Results - Please Wait"),

    Current = filename:join(BaseDir, "current"),
    Dir = filename:join(BaseDir,
                        "run-" ++ systest_env:timestamp()),

    ok = filelib:ensure_dir(filename:join(Dir, "foo")),
    file:delete(Current),
    file:make_symlink(Dir, Current),

    {CT, NCT, Acc, _} =
        lists:foldl(
            fun (M,{CovTot, NotCovTot, Acc, Cnt}) ->
                Cnt2 = if Cnt > 79 -> io:nl(), 0;
                              true -> Cnt + 1
                       end,
                io:format("."),
                case cover:analyze(M, module) of
                    {ok, {M, {Cov, NotCov}}=Result} ->
                        case analyse_to_file(M, Dir) of
                            {error, Reason} ->
                                systest_log:log(framework,
                                                "unable to generate html"
                                                " coverage report for ~p: ~p~n",
                                                [M, Reason]);
                            _ ->
                                ok
                        end,
                        {CovTot+Cov, NotCovTot+NotCov, [Result|Acc], Cnt2};
                    Error ->
                        throw(Error)
                end
            end, {0, 0, [], 0}, lists:sort(cover:modules())),
    io:format("~n~n"),
    Entries = lists:reverse(
                [report_entry(Cov, NotCov, M) || {M, {Cov, NotCov}} <- Acc]) ++
                [report_entry(CT, NCT, 'TOTAL')],
    case proplists:get_value(quiet, Config, false) of
        true  ->
            systest_log:log("~s~n",
                            [systest_utils:proplist_format(Entries)]);
        false ->
            io:format(user, "~s~n",
                      [systest_utils:proplist_format(Entries)])
    end,
    timer:sleep(1000),
    ok = cover:export(Export),

    %% LastExport is a symlink
    LastExport = filename:join(filename:dirname(Export), "cover.export"),
    file:delete(LastExport),
    file:read_link(LastExport),
    file:make_symlink(Export, LastExport),
    cover:reset(),
    ok.

%%
%% Private/Internal API
%%

analyse_to_file(Mod, Dir) ->
    cover:analyze_to_file(Mod,
            filename:join(Dir, atom_to_list(Mod) ++ ".html"), [html]).

report_entry(Cov, NotCov, Mod) ->
    S = io_lib:format(
        "~6.2f",
        [if
           Cov+NotCov > 0 -> 100.0*Cov/(Cov+NotCov);
           true -> 100.0
        end]),
    {Mod, lists:flatten(S)}.
