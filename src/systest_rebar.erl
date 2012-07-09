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
-module(systest_rebar).

-export([systest/2]).

%%
%% Public (Callable) Rebar API
%%

systest(Config, _) ->
    systest:start(),

    %% because sometimes, code that is accessed from an escript archive doesn't
    %% get handled in a particularly useful way by the code server.... :/
    rebar_log:log(debug, "~p~n", [code:ensure_loaded(systest_utils)]),
    
    % code:add_pathz(filename:dirname(filename:absname(code:which(systest)))),
    DepsDir = rebar_config:get_local(Config, deps_dir,
                        rebar_config:get_global(deps_dir, "deps")),
    code:add_patha(filename:join([rebar_utils:get_cwd(),
                                  DepsDir, "systest", "ebin"])),

    {ok, _} = net_kernel:start([systest_master, shortnames]),

    %% TODO: consider adding a time stamp to the scratch
    %%       dir like common test does
    ScratchDir = case os:getenv("SYSTEST_SCRATCH_DIR") of
                     false -> filename:join(temp_dir(), "systest");
                     Dir   -> Dir
                 end,

    systest_utils:rm_rf(ScratchDir),
    filelib:ensure_dir(filename:join([ScratchDir, "ct-logs", "foo"])),
    rebar_config:set_global(scratch_dir, ScratchDir),

    Profile = case os:getenv("SYSTEST_PROFILE") of
                  false -> os:getenv("USER");
                  Name -> Name
              end,
    Spec    = case rebar_utils:find_files("profiles", Profile ++ "\\.spec$") of
                  [SpecFile] -> SpecFile;
                  _          -> filename:join("profiles", "default.spec")
              end,

    case filelib:is_regular(Spec) of
        false ->
            rebar_utils:abort("No Test Specification Available~n"
                              "Suggested Profile: ~s~n"
                              "Discovered Test Specification: ~s~n",
                              [Profile, Spec]);
        true ->
            Env = [{scratch_dir, ScratchDir}|clean_config_dirs(Config)] ++
                    rebar_env() ++ os_env(),
            {ok, SpecOutput} = transform_file(Spec, temp_dir(), Env),
            {ok, FinalSpec} = process_config_files(ScratchDir,
                                                   SpecOutput, Env),

            CoverBase = filename:join(ScratchDir, "cover"),
            {ok, Export} = start_cover(CoverBase, Config),
            rebar_log:log(debug, "cover:modules() = ~p~n", [cover:modules()]),

            start_systest_logging(ScratchDir, Config),

            Result = ct:run_test([{'spec', FinalSpec},
                                  {logdir,
                                       filename:join(ScratchDir, "ct-logs")},
                                  {auto_compile, false}]),

            report_cover(CoverBase, Export, Config),

            case Result of
                {error, Reason} ->
                    error(Reason);
                _Results ->
                    case application:get_env(systest, failures) of
                        {ok, 0} ->
                            ok;
                        {ok, Failed} ->
                            rebar_utils:abort("Failed (~p failing test cases)~n",
                                              [Failed])
                    end
            end
    end.

start_systest_logging(ScratchDir, Config) ->
    ok = systest_log:start(system, systest_ct_log, common_test),
    ok = systest_log:start(framework, systest_ct_log, common_test),
    Active = rebar_config:get_local(Config, systest_active_loggers, []),
    [begin
        rebar_log:log(debug, "Activating Logging SubSystem ~p~n", [SubSystem]),
        case Target of
            glog ->
                Dest = filename:join(ScratchDir,
                                     atom_to_list(SubSystem) ++ ".log"),
                ok = systest_log:start_file(SubSystem, systest_log, Dest);
            flog ->
                ok = systest_log:start(SubSystem, systest_log, devnull);
            Name ->
                ok = systest_log:start(SubSystem, systest_log, Name)
        end
     end || {SubSystem, Target} <- Active],
    ok.

report_cover(Dir, Export, Config) ->
    rebar_log:log(info, "Code Coverage Results:~n", []),
    Summary = case rebar_config:get_local(Config, cover_summary, user) of
                  console -> user;
                  user    -> user;
                  File    -> Path = filename:join(rebar_utils:get_cwd(), File),
                             {ok, Fd} = file:open(Path, [write]),
                             Fd
              end,
    ok = filelib:ensure_dir(filename:join(Dir, "foo")),
    lists:foreach(fun (F) -> file:delete(F) end,
                  filelib:wildcard(filename:join(Dir, "*.html"))),
    {CT, NCT} =
        lists:foldl(
            fun (M,{CovTot, NotCovTot}) ->
                rebar_log:log(debug, "cover analysing ~p~n", [M]),
                case cover:analyze(M, module) of
                    {ok, {M, {Cov, NotCov}}} ->
                        ok = report_coverage_percentage(Summary,
                                                        Cov, NotCov, M),
                        case analyse_to_file(M, Dir) of
                            {error, Reason} ->
                                rebar_log:log(warn,
                                              "unable to generate html"
                                              " coverage report for ~p: ~p~n",
                                              [M, Reason]);
                            _ ->
                                ok
                        end,
                        {CovTot+Cov, NotCovTot+NotCov};
                    Error ->
                        throw(Error)
                end
            end, {0, 0}, lists:sort(cover:modules())),
    ok = report_coverage_percentage(user, CT, NCT, 'TOTAL'),
    timer:sleep(1000),
    % ok = file:close(SummaryFile),
    ok = cover:export(Export),
    cover:reset(),
    ok.

analyse_to_file(Mod, Dir) ->
    cover:analyze_to_file(Mod,
            filename:join(Dir, atom_to_list(Mod) ++ ".html"), [html]).

report_coverage_percentage(File, Cov, NotCov, Mod) ->
    io:format(File, "~6.2f ~p~n",
              [if
                   Cov+NotCov > 0 -> 100.0*Cov/(Cov+NotCov);
                   true -> 100.0
               end,
               Mod]).

start_cover(CoverBase, Config) ->
    cover:start(),

    CoverData = filename:join(CoverBase, "data"),
    ImportData = systest_utils:find(CoverData, ".*\\.cover\$"),
    SearchDirs = rebar_config:get_local(Config, cover_dirs, ["ebin"]),
    Cwd = rebar_utils:get_cwd(),
    Dirs = [case filename:pathtype(Dir) of
                relative -> filename:join(Cwd, Dir);
                absolute -> Dir
            end || Dir <- SearchDirs],

    [begin
         rebar_log:log(info, "cover compiling modules in ~s~n", [D]),
         Results = cover:compile_beam_directory(D),
         case [R || R <- Results, element(1, R) =:= error] of
             []     -> [rebar_log:log(debug, "compiled ~p~n", [M]) ||
                                M <- Results];
             Errors -> %error(Errors)
                       %% The effort involved in fixing this isn't worth it
                       %% given that we're coming off rebar shortly
                       argh
         end
     end || D <- Dirs],

    [begin
         rebar_log:log(info, "importing cover data from ~s~n", [F]),
         ok = cover:import(F)
     end || F <- ImportData],

    CoverFile = "systest-" ++ systest_utils:timestamp() ++ ".cover",
    Export = filename:join(CoverData, CoverFile),
    filelib:ensure_dir(Export),
    {ok, Export}.

process_config_files(ScratchDir, TempSpec, Env) ->
    {ok, Terms} = file:consult(TempSpec),
    {[Configs], Rest} = proplists:split(Terms, [config]),
    rebar_log:log(debug, "Processing config sections: ~p~n", [Configs]),
    Replacements = [begin
                        {ok, Path} = transform_file(F, ScratchDir, Env),
                        {config, Path}
                    end || {_, F} <- lists:flatten(Configs)],
    Spec = filename:join(ScratchDir, filename:basename(TempSpec)),
    {ok, Fd} = file:open(Spec, [append]),
    Content = Replacements ++ Rest,
    rebar_log:log(debug, "Write to ~s: ~p~n", [Spec, Content]),
    write_terms(Content, Fd),
    {ok, Spec}.

transform_file(File, ScratchDir, Env) ->
    case filelib:is_regular(File) of
        false -> rebar_utils:abort("File ~s not found.~n", [File]);
        true  -> ok
    end,
    Output = filename:join(ScratchDir, filename:basename(File)),
    Target = filename:absname(Output),
    Origin = filename:absname(File),
    rebar_log:log(info, "transform ~s into ~s~n", [Origin, Target]),

    %% this looks *pointless* but avoids calling dict:to_list/1
    %% unless it is actually going to use the result
    case rebar_log:get_level() of
        debug -> rebar_log:log(debug, "template environment: ~p~n", [Env]);
        _     -> ok
    end,

    Context = rebar_templater:resolve_variables(Env, dict:new()),
    {ok, Bin} = file:read_file(File),
    Rendered = rebar_templater:render(Bin, Context),

    file:write_file(Output, Rendered),
    {ok, Target}.

write_terms(Terms, Fd) ->
    try
        [begin
            Element = lists:flatten(erl_pp:expr(erl_parse:abstract(Item))),
            Term = Element ++ ".\n",
            ok = file:write(Fd, Term)
         end || Item <- Terms]
    after
        file:close(Fd)
    end.

temp_dir() ->
    %% TODO: move this into hyperthunk/rebar_plugin_manager?
    case os:type() of
        {win32, _} ->
            %% mirrors the behaviour of the win32 GetTempPath function...
            get("TMP", get("TEMP", element(2, file:get_cwd())));
        {unix, darwin} ->
            "/tmp";  %% unfeasibly long paths can occur otherwise...
        _ ->
            case os:getenv("TMPDIR") of
                false -> "/tmp"; %% this is what the JVM does, but honestly...
                Dir   -> Dir
            end
    end.

get(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Value
    end.

clean_config_dirs(Config) ->
    [{plugin_dir, rebar_config:get_local(Config, plugin_dir, "")}] ++
    lower_case_key_names(rebar_config:get_env(Config, rebar_deps)).

lower_case_key_names(Items) ->
    [{string:to_lower(K), V} || {K, V} <- Items].

rebar_env() ->
    [{base_dir, rebar_config:get_global(base_dir, rebar_utils:get_cwd())}] ++
    clean_env(application:get_all_env(rebar_global)).

os_env() ->
    systest_config:get_env().

clean_env(Env) ->
   [ E || {_, [H|_]}=E <- Env, is_integer(H) ].

