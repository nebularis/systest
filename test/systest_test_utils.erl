%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
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
-module(systest_test_utils).

-compile(export_all).

wait_do(Timeout, Do) ->
    fun(_) ->
            Ms = systest_utils:time_to_ms(Timeout),
            systest:log("sleeping for ~pms~n", [Ms]),
            timer:sleep(Ms),
            Do()
    end.

perm() ->
    receive
        stop ->
            exit(shutdown);
        {exit, How} ->
            exit(How);
        {exit, From, How2} ->
            From ! {ok, self(), exiting, How2},
            exit(How2);
        Other ->
            systest:log("perm received ~p~n", [Other]),
            perm()
    end.

wait_for(Choice) ->
    wait_for(Choice, infinity).

wait_for(Choice, Timeout) ->
    receive
        X when is_function(Choice) ->
            case Choice(X) of
                ignore ->
                    wait_for(Choice, Timeout);
                {exit, What} ->
                    exit(What);
                {return, What} ->
                    What;
                return ->
                    X
            end;
        X2 when X2 == Choice ->
            X2
    after Timeout ->
            {timeout, Timeout}
    end.

choose(X) when is_function(X) ->
    fun(X1) ->
            case X(X1) of
                true  -> {return, X1};
                false -> ignore;
                Other -> Other
            end
    end;
choose(X) ->
    fun(X1) when X1 == X -> {return, X};
       (_)               -> ignore
    end.

allow(X) when is_function(X) ->
    fun(X1) ->
            case X(X1) of
                true  -> ignore;
                false -> {exit, {unexpected_message, X1}}
            end
    end;
allow(X) ->
    fun(X1) when X1 == X -> ignore;
       (X2)              -> {exit, {unexpected_message, X2}}
    end.

return(X) ->
    fun(X1) ->
            case (allow(X))(X1) of
                ignore ->
                    {return, X1};
                _Other ->
                    {exit, {unexpected_message, X1}}
            end
    end.

disallow(X) ->
    fun(X1) when X1 == X -> {exit, {disallowed_message, X1}};
       (_)               -> ignore
    end.

choice(X, More) ->
    Choice = choose(X),
    fun(X1) ->
            case Choice(X1) of
                ignore -> More(X1);
                What   -> What
            end
    end.

match_element(Elem, Match) ->
    fun(X) when is_tuple(X) ->
            element(Elem, X) == Match;
       (_) ->
            false
    end.

test_element(Elem, Test) ->
    fun(X) when is_tuple(X) ->
            Test(element(Elem, X));
       (_) ->
            false
    end.

check_log_dir() ->
    init:get_plain_argument(log_dir).
