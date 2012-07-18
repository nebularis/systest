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
%% Logging Event Handler - provides a gen_event based logging infrastructure
%% which is very lightweight, at relatively low cost. Whilst we aren't going to
%% win any prizes for efficiency, we <i>only</i> deal with logging test related
%% information, so we can probably afford to be relatively lazy here. This 
%% approach also enables us to remain basically framework agnostic, and does
%% allow a user (in theory) to provide a custom log handler that delegates to
%% their own logging framework of choice.
%% @end
%% -----------------------------------------------------------------------------
-module(systest_log).

%% TODO: this might just become a bottle neck at some point

-behaviour(gen_event).

-export([behaviour_info/1]).
-export([start_link/0]).
-export([start/0, start/1, start/3, start_file/2, start_file/3]).
-export([log/2, log/3]).
-export([write_log/4]).
-export([activate_logging_subsystem/3]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {id, mod, fd}).

%%
%% Public API
%%

%% @hidden
behaviour_info(callbacks) ->
    [{write_log, 4}];
behaviour_info(_) ->
    undefined.

start_link() ->
    %% TODO: pass the log_base here, and use it when activating handlers....
    gen_event:start_link({local, systest_event_log}).

%% @doc Starts the default systest logging handler: 'system'.
%% @end
start() ->
    start(user).

start(File) ->
    case is_list(File) of
        true  -> start_file(system, File);
        false -> start(system, ?MODULE, File)
    end.
    
%% @doc Starts a logging handler registered with 'Id', that outputs to File.
%% @end
start_file(Id, File) ->
    start_file(Id, ?MODULE, File).

%% @doc as start_file/2, but takes a callback module.
%% @end
start_file(Id, Mod, Path) when is_list(Path) ->
    {ok, IoDevice} = file:open(Path, [write]),
    start_file(Id, Mod, IoDevice);
start_file(Id, Mod, Dest) ->
    start(Id, Mod, Dest).

%% @doc Starts a log handler registered with Id, using the callback module
%% Mod and the io-device (i.e., file descriptor or registered io handling
%% process name) Output.
%% @end
start(Id, Mod, Output) ->
    gen_event:add_handler(systest_event_log, {?MODULE, Id}, [Id, Mod, Output]).

activate_logging_subsystem(SubSys, Id, LogBase) ->
    Handlers = gen_event:which_handlers(systest_event_log),
    case [H || {_, H} <- Handlers, H =:= SubSys] of
        [] ->
            {error, not_installed};
        [H|_] ->
            gen_event:delete_handler(systest_event_log,
                                     {?MODULE, {framework, Id}}, []),
            case gen_event:call(systest_event_log, {?MODULE, H}, where) of
                devnull ->
                    File = filename:join(LogBase,
                                  atom_to_list(Id) ++ ".log"),
                    ok = start_file({framework, Id}, File),
                    {ok, File};
                system ->
                    %% NB: we *only* allow 1 log handler for 'system' events
                    {error, system};
                Dest ->
                    ok = start({framework, Id}, ?MODULE, Dest),
                    {ok, Dest}
            end
    end.

%% @doc Writes to the logging handler Scope, formatting Fmt with Args. This
%% should work in much the same way as io:format/2 does, although this is
%% implementation dependent to some extent.
%% @end
log(Scope, Fmt, Args) ->
    %% NB: we avoid gen_event:call/3 because it requires {Module, Id} to be
    %% maintained *somewhere* in order to identify a recipient, which is about
    %% as useful as a chocolate teapot here. Instead we match on id in 
    %% the handle_event callback and filter out unwanted messages that way.
    gen_event:sync_notify(systest_event_log, {Scope, Fmt, Args}).

%% @doc Writes (with formatting as per log/3) to the system logging handler.
%% @end
log(Fmt, Args) ->
    log(system, Fmt, Args).

%%
%% systest_log callback API!
%%

write_log({framework, EvId}, Fd, What, Args) ->
    write_log(EvId, Fd, What, Args);
write_log(EvId, Fd, What, Args) ->
    io:format(Fd, "[~p]  " ++ What, [EvId|Args]).

%%
%% OTP gen_event API
%%

init([Id, Mod, Fd]) ->
    {ok, #state{id=Id, mod=Mod, fd=Fd}}.

handle_event({Scope, Fmt, Args},
             State=#state{id=Id, mod=Mod, fd=Fd}) when Scope == Id ->
    write(Mod, Fd, Id, Fmt, Args),
    {ok, State};
handle_event({Fmt, Args}, State=#state{id=system, mod=Mod, fd=Fd}) ->
    write(Mod, Fd, system, Fmt, Args),
    {ok, State};
handle_event(_Message, State) ->
    {ok, State}.

handle_call(where, State) ->
    {ok, State#state.fd, State};
handle_call(_, State) ->
    {ok, ignored, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write(_, devnull, _, _, _) ->
    ok;
write(Mod, Fd, EvId, Fmt, Args) ->
    catch(Mod:write_log(EvId, Fd, Fmt, Args)).
