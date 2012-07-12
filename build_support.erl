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

-export(['publish-wiki'/2, post_doc/2, mv_test_beams/2]).

post_doc(Config, _) ->
    Dest = doc_dir(Config),
    WikiDir = wiki_dir(Config),
    case filelib:is_dir(WikiDir) of
        true ->
            Generated = doc_files(Dest),
            rebar_file_utils:cp_r(Generated, WikiDir);
        false ->
            rebar_log:log(info, "Skipping ~p:post_doc/2 as no "
                                "wiki directory was found~n", [?MODULE])
    end,
    ok.

'publish-wiki'(Config, _) ->
    Dest = wiki_dir(Config),
    Generated = lists:map(fun filename:basename/1, doc_files(Dest)),
    Files = string:join(Generated, " "),
    rebar_utils:sh("git add " ++ Files, [{cd, Dest}]),
    rebar_utils:sh("git ci -m 'updated by rebar...'", [{cd, Dest}]),
    rebar_utils:sh("git push origin master", [{cd, Dest}]),
    ok.

mv_test_beams(_, _) ->
    %% Because rebar WILL NOT output beams into a directory other than 'ebin'
    Base = rebar_config:get_global(base_dir,
                            rebar_utils:get_cwd()),
    TestSources = filelib:wildcard(
                        filename:join([Base, "test", "*.erl"])),
    [begin
         Target = filename:basename(Src, ".erl") ++ ".beam",
         Source = filename:join([Base, "ebin", Target]),
         Dest = filename:join([Base, "test-ebin", Target]),
         rebar_utils:ensure_dir(Dest),
         case filelib:is_regular(Source) of
             true  -> ok = file:rename(Source, Dest);
             false -> ok
         end
     end || Src <- TestSources],
    ok.

wiki_dir(Config) ->
    filename:absname(rebar_config:get_global(wiki_repo,
            rebar_config:get_local(Config, wiki_repo, "../systest.wiki"))).

doc_dir(Config) ->
    filename:absname(proplists:get_value(dir,
                rebar_config:get_local(Config, edoc_opts, []), "doc")).

doc_files(DocDir) ->
    filelib:wildcard(filename:join(DocDir, "*.*")) -- 
                    [filename:join(DocDir, "README.md"),
                     filename:join(DocDir, "TOC.md")].
