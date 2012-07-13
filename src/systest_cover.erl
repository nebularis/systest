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
-module(systest_cover).

-include("systest.hrl").

-export([start/2, report_cover/3]).

-spec start(file:filename(),
            systest_config:config()) -> {'ok', file:filename()} |
                                        {error, term()}.
start(ScratchDir, Config) ->
    cover:start(),

    CoverBase = filename:join(ScratchDir, "cover"),
    CoverData = filename:join(CoverBase, "data"),
    ImportData = case ?CONFIG('cover-import', Config, false) of
                     true ->
                         systest_utils:find_files(CoverData, ".*\\.cover\$");
                     false ->
                         []
                 end,
    SearchDirs = ?CONFIG(cover_dirs, Config, ["ebin"]),
    {ok, Cwd} = file:get_cwd(),
    Dirs = [case filename:pathtype(Dir) of
                relative -> filename:join(Cwd, Dir);
                absolute -> Dir
            end || Dir <- SearchDirs],

    [begin
         systest_log:log(framework, "cover compiling modules in ~s~n", [D]),
         Results = cover:compile_beam_directory(D),
         case [R || R <- Results, element(1, R) =:= error] of
             []     -> ok;
             Errors -> error(Errors)
         end
     end || D <- Dirs],

    [begin
         systest_log:log(framework, "importing cover data from ~s~n", [F]),
         ok = cover:import(F)
     end || F <- ImportData],

    CoverFile = "systest-" ++ systest_env:timestamp() ++ ".cover",
    Export = filename:join(CoverData, CoverFile),
    filelib:ensure_dir(Export),
    {ok, Export}.

-spec report_cover(file:filename(),
                   file:filename(),
                   systest_config:config()) -> ok.
report_cover(Dir, Export, Config) ->
    systest_utils:print_heading("Code Coverage Results"),
    {Summary, SummaryFile} = 
            case ?CONFIG('cover-summary', Config, user) of
                console -> {user, none};
                user    -> {user, none};
                File    -> {ok, Cwd} = file:get_cwd(),
                           Path = filename:join(Cwd, File),
                           {ok, Fd} = file:open(Path, [write]),
                           {Fd, Path}
              end,
    try
        ok = filelib:ensure_dir(filename:join(Dir, "foo")),
        lists:foreach(fun (F) -> file:delete(F) end,
                      filelib:wildcard(filename:join(Dir, "*.html"))),
        {CT, NCT, Acc} =
            lists:foldl(
                fun (M,{CovTot, NotCovTot, Acc}) ->
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
                            {CovTot+Cov, NotCovTot+NotCov, [Result|Acc]};
                        Error ->
                            throw(Error)
                    end
                end, {0, 0, []}, lists:sort(cover:modules())),

        [report(Summary, Cov, NotCov, M) || {M, {Cov, NotCov}} <- Acc],
        ok = report(Summary, CT, NCT, 'TOTAL')
    after
        if Summary /= user ->
            ok = file:close(Summary),
            {ok, Bin} = file:read_file(SummaryFile),
            io:format("~s~n", [Bin]);
        true ->
            ok
        end
    end,
    timer:sleep(1000),
    ok = cover:export(Export),
    cover:reset(),
    ok.

%%
%% Private/Internal API
%%

analyse_to_file(Mod, Dir) ->
    cover:analyze_to_file(Mod,
            filename:join(Dir, atom_to_list(Mod) ++ ".html"), [html]).

report(File, Cov, NotCov, Mod) ->
    io:format(File, "~6.2f ~p~n",
              [if
                   Cov+NotCov > 0 -> 100.0*Cov/(Cov+NotCov);
                   true -> 100.0
               end,
               Mod]).
