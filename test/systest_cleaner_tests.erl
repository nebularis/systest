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
-module(systest_cleaner_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

kill_some_maybe_dead_test() ->
    Pids = [spawn(fun short_loop/0) || _ <- lists:seq(1, 100000)],
    timer:sleep(1000),
    erlang:yield(),
    systest_cleaner:kill_wait(Pids, fun kill_it/1),
    [?assertEqual(false, erlang:is_process_alive(P)) || P <- Pids],
    true.

kill_with_timeout_test_() ->
    {timeout, 60,
     fun() ->
             Pids = [spawn(fun loop/0) || _ <- lists:seq(1, 100) ],
             case systest_cleaner:kill_wait(Pids,
                                            fun kill_it_slowly/1, 10000) of
                 {error, timeout, Survivors} ->
                     ?assert(length(Survivors) > 0);
                 Other ->
                     ?assertEqual(ok, Other)
             end
    end}.

kill_many_test() ->
    Pids = [ spawn(fun loop/0) || _ <- lists:seq(1, 100) ],
    systest_cleaner:kill_wait(Pids, fun kill_it/1),
    [?assertEqual(false, erlang:is_process_alive(P)) || P <- Pids],
    true.

kill_it_slowly(Pid) ->
    timer:sleep(20000),
    exit(Pid, kill).

kill_it(Pid) ->
    exit(Pid, boom).

loop() ->
    loop().

short_loop() ->
    ok.
