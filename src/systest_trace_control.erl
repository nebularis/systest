%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
%% in the Software without restriction, including without limitation the rigwhts
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
%% Coordinator process for tracers on remote nodes and trace clients on the
%% test coordination node. As well as setting up supervision/monitoring of
%% these processes, this module provides a management API for starting,
%% and stopping them.
%% ----------------------------------------------------------------------------
-module(systest_trace_control).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(systest_utils, [safe_append/2]).

-record(remote, { pid    :: pid(),
                  args   :: [term()],
                  config :: term() }).

-record(state, { supervisor_pid :: pid(),
                 %% mapping from node() => #remote
                 remotes        :: dict() }).

%%
%% OTP gen_server API
%%

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, Pid} = systest_trace_sup:start_link(),
    {ok, #state{supervisor_pid = Pid,
                remotes = dict:new()}}.

handle_call({start, Node, Args}, _From, State=#state{remotes=Remotes}) ->
    {ok, _Child, Remotes2} = start(Node, Args, Remotes),
    {reply, ok, State#state{remotes=Remotes2}};
handle_call({configure, Node, Payload}, _From,
            State=#state{remotes=Remotes}) ->
    case configure(Node, Payload, Remotes) of
        {error, _}=Err ->
            {reply, Err, State};
        {ok, Remotes2} ->
            {reply, ok, State#state{remotes=Remotes2}}
    end;
handle_call({stop, Node}, _From, State=#state{remotes=Remotes}) ->
    case lookup_pid(Node, Remotes) of
        error ->
            {reply, not_running, State};
        Pid ->
            systest_log:log(framework,
                            "stopping trace on ~p~n", [Node]),
            ok = systest_trace_sup:stop_remote_trace(Pid),
            {ok, State#state{remotes=dict:erase(Node, Remotes)}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State=#state{remotes=Remotes}) ->
    case lookup(Node, Remotes) of
        error ->
            %% not tracking Node
            {noreply, State};
        #remote{pid=Pid, args=Args, config=Config}=Rem ->
            case rpc:call(Node, erlang,
                          is_process_alive, [Pid]) of
                false ->
                    {ok, Child, Remotes2} = start(Node, Args, Remotes),
                    %% configure/4 will overwrite the record that start/3
                    %% stores, but we need to ensure that the pid is updated
                    %% *and* we have to run the config on the remote node
                    %% all over again, hence our record gets 'reset' here...
                    Rem2 = Rem#remote{pid=Child, args=Args, config=[]},
                    {ok, Remotes3} = configure(Node, Config, Remotes2, Rem2),
                    {noreply, State#state{remotes=Remotes3}};
                _ ->
                    {noreply, State}
            end
    end;
handle_info({'EXIT', Pid, Reason},
            State=#state{supervisor_pid=SPid}) when Pid =:= SPid ->
    {stop, {supervisor, Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private/Internal API
%%

start(Node, Args, Remotes) ->
    %% TODO: handle return values consistently here, as
    %% this impacts on the user experience when enabled
    {ok, Child} = systest_trace_sup:start_remote_trace(Node, Args),
    Rem = #remote{pid=Child, args=Args, config=[]},
    Remotes2 = dict:store(Node, Rem, Remotes),
    {ok, Child, Remotes2}.

configure(Node, Payload, Remotes) ->
    case lookup(Node, Remotes) of
        error ->
            {error, not_running};
        Rem ->
            configure(Node, Payload, Remotes, Rem)
    end.

configure(Node, Payload, Remotes, #remote{pid=Pid, config=Config}=Rem) ->
    systest_trace_remote:configure(Pid, Payload),
    Remotes2 = dict:store(Node,
                          Rem#remote{config=safe_append(Config, Payload)},
                          Remotes),
    {ok, Remotes2}.

lookup_pid(Node, Remotes) ->
    case lookup(Node, Remotes) of
        error            -> error;
        #remote{pid=Pid} -> Pid
    end.

lookup(Node, Remotes) ->
    case dict:find(Node, Remotes) of
        error       -> error;
        {ok, Value} -> Value
    end.

