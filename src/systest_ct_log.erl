%% -------------------------------------------------------------------
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
%% @hidden
%% Module systest_ct_log - provides a logging callback handler that prints
%% to the common test log. Both raw and HTML outputs are generated this way.
%% @end
%% -----------------------------------------------------------------------------
-module(systest_ct_log).

-behaviour(systest_log).
-export([write_log/4, start/0, stop/0]).

-import(systest_utils, [as_string/1]).

start() ->
    ok = systest_log:start(ct, systest_ct_log, common_test).

stop() ->
    ok = systest_log:stop(ct, systest_ct_log, common_test).

%%
%% systest_log callback API!
%%

-ifndef(CT_FIXED).
write_log(framework, _, _, _) ->
    ok;
write_log({framework, _}, _, _, _) ->
    ok;
write_log(EvId, _Fd, What, Args) ->
    ct:log("[" ++ as_string(EvId) ++ "] " ++ as_string(What), Args).
-else.
write_log(EvId, _Fd, What, Args) ->
    ct:log("[" ++ as_string(EvId) ++ "] " ++ as_string(What), Args).
-endif.

