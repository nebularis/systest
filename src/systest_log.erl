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
-module(systest_log).

%% TODO: this is likely to become a bottle neck at some point, so we should
%% rework it, possibly using a second slave/io node to handle the throughput
%% of write intensive traffic whilst we deal with everything else

-behaviour(gen_event).

-export([behaviour_info/1]).

-export([start/1, start/3, start_file/2]).
-export([log/2, log/3, write_log/3]).
-export([ts/0, console/2]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id, handler, fd}).

%%
%% Public API
%%

behaviour_info(callbacks) ->
    [{write_log, 2}];
behaviour_info(_) ->
    undefined.

start(LogBase) ->
    start(fun ct:log/3, ct, systest),
    start_file(system, filename:join(LogBase, "system.log")).

start_file(Id, Fd) ->
    start(?MODULE, Id, Fd).

start(CallbackMod, Id, FilePath) when is_list(FilePath) ->
    filelib:ensure_dir(FilePath),
    {ok, IoDevice} = file:open(FilePath, [write]),
    gen_event:add_handler(systest_event_log, {?MODULE, Id},
                          [Id, CallbackMod, IoDevice]);
start(Handler, Id, Where) ->
    gen_event:add_handler(systest_event_log, {?MODULE, Id},
                          [Id, Handler, Where]).

log(Scope, Fmt, Args) ->
    gen_event:call(systest_event_log, Scope, {Fmt, Args}).

log(Fmt, Args) ->
    gen_event:notify(systest_event_log, {Fmt, Args}).

console(Msg, Args) ->
    io:format(user, "[systest] " ++ Msg, Args).

ts() ->
    Now = now(),
    {{Year,Month,Day}, {Hour,Min,Sec}} = calendar:now_to_local_time(Now),
    MilliSec = trunc(element(3, Now)/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec])).

%%
%% Private API
%%

slog(Fd, What, Args) ->
    write_log(Fd, ts() ++ " - " ++ What, Args).

write_log(Fd, port, Data) ->
    io:write(Fd, Data);
write_log(Fd, What, Args) ->
    io:format(Fd, What, Args).

%%
%% OTP gen_event API
%%

init([Id, Handler, IoDevice]) ->
    {ok, #state{id=Id, handler=Handler, fd=IoDevice}}.

handle_event({Fmt, Args},
             State=#state{handler=Handler, fd=Fd})
                    when is_function(Handler, 3) ->
    Handler(Fd, Fmt, Args),
    {ok, State};
handle_event({Fmt, Args},
             State=#state{handler=Mod, fd=Fd}) when is_atom(Mod) ->
    Mod:write_log(Fd, Fmt, Args),
    {ok, State};
handle_event(_Message, State) ->
    {ok, State}.

handle_call({Fmt, Args},
            State=#state{handler=Handler, fd=Fd})
                    when is_function(Handler, 3) ->
    {ok, Handler(Fd, Fmt, Args), State};
handle_call({Fmt, Args},
            State=#state{handler=Mod, fd=Fd}) when is_atom(Mod) ->
    {ok, Mod:write_log(Fd, Fmt, Args), State};
handle_call(_, State) ->
    {ok, ignored, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, #state{fd=Fd}) when Fd =/= undefined ->
    file:close(Fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

