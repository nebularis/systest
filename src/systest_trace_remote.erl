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
-module(systest_trace_remote).

-behaviour(gen_server).

%% API exports
-export([start_remote/2]).
-export([start_link/1]).
-export([configure/2]).

%% gen_server exports

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { trace_port :: port(),
                 port_fun   :: fun(),
                 ip_port    :: integer(),
	         qsize      :: integer(),
	         parent     :: pid() }).

-define(DEFAULT_QSIZE, 50).

%%
%% Public API
%%

start_remote(Node, Args) ->
    ok = systest_utils:remote_load_module(Node, ?MODULE),
    rpc:call(Node, ?MODULE, start_link, Args).

start_link(Args) ->
    gen_server:start_link({local, systest_tracer},
			  ?MODULE, Args, []).

configure(Pid, Payload) ->
    gen_server:cast(Pid, {configure, Payload}).

%%
%% OTP gen_server API
%%

init([Parent, IpPort]) ->
    init([Parent, IpPort, ?DEFAULT_QSIZE]);
init([Parent, IpPort, QSize]) ->
    process_flag(trap_exit, true),
    PortFun = dbg:trace_port(ip, {IpPort, QSize}),
    Port = PortFun(),
    link(Port),
    {ok, #state{ ip_port=IpPort,
	         port_fun=PortFun,
	         trace_port=Port,
	         qsize=QSize,
	         parent=Parent }}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({enable, PidSpec, Flags}, State=#state{trace_port=Port}) ->
    do_trace(PidSpec, true, Flags, Port),
    {noreply, State};
handle_cast({disable, PidSpec, Flags}, State=#state{trace_port=Port}) ->
    do_trace(PidSpec, false, Flags, Port),
    {noreply, State};
handle_cast({set, MFA, MatchSpec}, State) ->
    do_tp(MFA, MatchSpec, []),
    {noreply, State};
handle_cast({clear, MFA, Flags}, State) ->
    do_tp(MFA, false, Flags),
    {noreply, State};
handle_cast(flush, State=#state{trace_port=Port}) ->
    trace_port_flush(Port),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

do_trace(PidSpec, How, Flags, Port) ->
    erlang:trace(PidSpec, How, [{tracer, Port}|Flags]).

handle_info({'EXIT', Pid, shutdown}, State) when Pid =:= self() ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

do_tp(MFA, MatchSpec, Flags) ->
    erlang:trace_pattern(MFA, MatchSpec, Flags).

trace_port_flush(Port) ->
    Ref = erlang:trace_delivered(all),
    receive
	{trace_delivered, all, Ref} -> ok
    end,
    %% this little gem comes from dbg.erl
    erlang:port_control(Port, $f, "").

