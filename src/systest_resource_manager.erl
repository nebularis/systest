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
%%
%% Provides a resource manager for the systest application, ensuring that the
%% following invariants hold
%%
%% 1. all resources are fully initialised before use
%% 2. all resources are fully disposed of once they're out of scope
%% 3. 'singleton' resources can not have more than one instance at a time
%% 4. singletons are locked whilst they're initialising or disposing
%% 5. permanently locked singleton resources can never be used again
%%
%% All resources enter a <em>locked</em> state whilst they're starting and/or
%% stopping. Resources become permanently locked if they remain the this state
%% for longer than the max_locked_timeout value for the executing test profile.
%% The value of max_locked_timeout is set to 'infinity' by default however.
%%
%% Clients can choose to wait for a (non-permanent) lock to be released and
%% can do so for a configurable timeout or 'infinity'. Once a resource becomes
%% permanently locked however, any clients waiting for it are rudely ejected.
%%
%% @end
%% ----------------------------------------------------------------------------
-module(systest_resource_manager).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-compile(export_all).

%%
%% Public API
%%

require(ResourceName) ->
    %% TODO: get the ID and check state
    {ok, ResourceName}.

wait(Resource) ->
    wait(Resource, infinity).

wait(Resource, Timeout) ->
    %% wait + timeout
    {ok, Resource, Timeout}.

%%
%% OTP gen_server API
%%

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

