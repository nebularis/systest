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
-export([write_log/4, start/0]).

start() ->
    ok = systest_log:start(system, systest_ct_log, common_test),
    ok = systest_log:start(framework, systest_ct_log, common_test),
    ok = systest_log:start(operator, systest_ct_log, common_test),
    ok = systest_log:start(sut, systest_ct_log, common_test),
    ok = systest_log:start(process, systest_ct_log, common_test).

%%
%% systest_log callback API!
%%

write_log(_EvId, _Fd, What, Args) ->
    ct:log(What, Args).
