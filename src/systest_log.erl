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
-module(systest_log).

%% TODO: this is likely to become a bottle neck at some point, so we should
%% rework it, possibly using a second slave/io node to handle the throughput
%% of write intensive traffic whilst we deal with everything else

-behaviour(gen_event).

-export([behaviour_info/1]).

-export([start/0, start/1, start/2, start_file/1]).
-export([log/2, log/3]).
-export([write_log/2]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {mod, fd}).

%%
%% Public API
%%

behaviour_info(callbacks) ->
    [{write_log, 2}];
behaviour_info(_) ->
    undefined.

start() ->
    start(?MODULE).

start_file(Fd) ->
    start(?MODULE, Fd).

start(CallbackMod, Output) ->
    {ok, IoDevice} = file:open(Output, [write]),
    gen_event:add_handler(systest_event_log, ?MODULE, [CallbackMod, IoDevice]).

start(CallbackMod) when is_atom(CallbackMod) ->
    gen_event:add_handler(systest_event_log, ?MODULE, [CallbackMod]).

%%
%% systest_log callback API!
%%

log(Scope, Fmt, Args) ->
    gen_event:notify(systest_event_log, {Scope, Fmt, Args}).

log(Fmt, Args) ->
    gen_event:notify(systest_event_log, {Fmt, Args}).

%%
%% systest_log callback API!
%%

write_log(?MODULE, {Where, What, Args}) ->
    ct:log(Where, What, Args);
write_log(?MODULE, {What, Args}) ->
    ct:log(What, Args);
write_log(Fd, {_Where, What, Args}) ->
    file:write(Fd, io_lib:format(What, Args));
write_log(Fd, {port, Data}) ->
    file:write(Fd, Data).

%%
%% OTP gen_event API
%%

init([CallbackMod, IoDevice]) ->
    {ok, #state{mod=CallbackMod, fd=IoDevice}};
init([CallbackMod]) ->
    {ok, #state{mod=CallbackMod}}.

handle_event(Event, State=#state{mod=CallbackMod, fd=undefined}) ->
    CallbackMod:write_log(?MODULE, Event),
    {ok, State};
handle_event(Event, State=#state{mod=CallbackMod, fd=Fd}) ->
    CallbackMod:write_log(Fd, Event),
    {ok, State};
handle_event(_Message, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ignored, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
