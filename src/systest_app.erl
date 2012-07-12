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
-module(systest_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%
%% Application callbacks
%%

start(_StartType, _StartArgs) ->
    case catch( escript:script_name() ) of
        {'EXIT', _} ->
            %% NB: code:lib_dir/2 doesn't *always* work - e.g., when we're in
            %% in fact testing our own code base....
            AppF = code:where_is_file("systest.app"),
            AbsEbin = filename:absname(filename:dirname(AppF)),
            Path = filename:join([filename:dirname(AbsEbin),
                                 "priv", "banner.txt"]),
            {ok, Bin} = file:open(Path, [read]),
            application:set_env(systest, banner, Bin);
        Path when is_list(Path) ->
            ok
    end,
    systest_sup:start_link().

stop(_State) ->
    ok.
