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
-module(build_support).

-export([pre_compile/2, post_compile/2]).
-export(['publish-wiki'/2, post_doc/2]).

pre_compile(Config, _) ->
    case is_base_dir(Config) of
        true ->
            {ok, [{env, Env}]} = file:consult(
                                    filename:join("build", "app.env")),
            {ok, Banner} = file:read_file(
                                    filename:join("build", "banner.txt")),
            AppEnv = lists:keystore(banner, 1, Env, {banner, Banner}),
            AppVars = {env, AppEnv},            
            ok = file:write_file("app.vars",
                        io_lib:format("~p.\n", [AppVars]), [write]);
        false ->
            rebar_log:log(debug, "skipping app.vars generation in ~s~n",
                          [rebar_utils:get_cwd()]),
            ok
    end.

post_compile(Config, _) ->
    case is_base_dir(Config) of
        true  -> file:delete("app.vars");
        false -> ok
    end,
    ok.

post_doc(Config, _) ->
    Dest = doc_dir(Config),
    WikiDir = wiki_dir(Config),
    case filelib:is_dir(WikiDir) of
        true ->
            Generated = doc_files(Dest),
            rebar_file_utils:cp_r(Generated, WikiDir),
            rebar_file_utils:cp_r(static_files(rebar_utils:get_cwd()),
                                  WikiDir);
        false ->
            rebar_log:log(info, "Skipping ~p:post_doc/2 as no "
                                "wiki directory was found~n", [?MODULE])
    end,
    ok.

'publish-wiki'(Config, _) ->
    Dest = wiki_dir(Config),
    BaseDir = rebar_utils:get_cwd(),
    Generated = lists:map(fun filename:basename/1, doc_files(Dest) ++
                                                   static_files(BaseDir)),
    Files = string:join(Generated, " "),
    rebar_utils:sh("git add " ++ Files, [{cd, Dest}]),
    rebar_utils:sh("git ci -m 'updated by rebar...'", [{cd, Dest}]),
    rebar_utils:sh("git push origin master", [{cd, Dest}]),
    ok.

wiki_dir(Config) ->
    filename:absname(rebar_config:get_global(Config, wiki_repo,
            rebar_config:get_local(Config, wiki_repo, "../systest.wiki"))).

doc_dir(Config) ->
    filename:absname(proplists:get_value(dir,
                rebar_config:get_local(Config, edoc_opts, []), "doc")).

static_files(BaseDir) ->
    filelib:wildcard(filename:join([BaseDir, "static", "*.*"])) ++ 
	[filename:join(BaseDir, "ROADMAP.md")].

doc_files(DocDir) ->
    filelib:wildcard(filename:join(DocDir, "*.*")) --
                    [filename:join(DocDir, "README.md"),
                     filename:join(DocDir, "TOC.md")].

is_base_dir(Config) ->
    rebar_utils:get_cwd() == rebar_config:get_xconf(Config,
                                                    base_dir, undefined).

