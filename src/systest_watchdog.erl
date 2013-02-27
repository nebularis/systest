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
-module(systest_watchdog).

-include("systest.hrl").

-import(systest_log, [log/2, log/3]).

-define(ETS_OPTS,
        [{read_concurrency, true},
         {write_concurrency, false}]).

-record(state, {
    sut_table,
    proc_table,
    exception_table,
    last_timeout = infinity
}).

-behaviour(gen_server).

%% API Exports

-export([start/0, sut_started/3, exceptions/1, reset/0, stop/0,
         proc_started/2, proc_stopped/2, force_stop/1, force_stop/2]).

-export([clear_exceptions/0, dump/0]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%
%% Public API
%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

sut_started(Id, Pid, Timeout) ->
    %% TODO: there *should* be a timeout here....
    gen_server:call(?MODULE, {sut_started, Id, Pid}, Timeout).

reset() ->
    gen_server:call(?MODULE, reset).

force_stop(Id) ->
    force_stop(Id, infinity).

force_stop(Id, Timeout) ->
    GenServerTimeout = if erlang:is_integer(Timeout) ->
                               erlang:round(Timeout * 1.5);
                          true ->
                               Timeout
                       end,
    case catch(gen_server:call(?MODULE, {force_stop, Id, Timeout},
                               GenServerTimeout)) of
        {error, regname, Id} ->
            log(framework, "ignoring stop for deceased SUT ~p~n", [Id]);
        {timeout, {Id, Pid}} ->
            exit(Pid, kill),
            {error, timeout};
        {timeout, _} ->
            {error, timeout};
        Result ->
            Result
    end.

proc_started(Cid, Pid) ->
    %% NB: we do *not* link to procs, as the sut *should*
    %% in theory be responsible for cleaning up its own procs
    %%
    %% NB: if we blocked on accessing the proc_table, we would
    %% very likely deadlock the system - just when automated
    %% cleanup is meant to be leaving a clean environment behind
    case ets:insert_new(proc_table, {{Cid, Pid}}) of
        false -> {error, duplicate_proc};
        true  -> ok
    end.

proc_stopped(Cid, Pid) ->
    %% NB: if we blocked on accessing the proc_table, we would
    %% very likely deadlock the system - just when automated
    %% cleanup is meant to be leaving a clean environment behind
    ets:delete(proc_table, {{Cid, Pid}}).

exceptions(SutId) ->
    gen_server:call(?MODULE, {exceptions, SutId}).

clear_exceptions() ->
    gen_server:call(?MODULE, clear_exceptions).

dump() ->
    catch gen_server:call(?MODULE, dump, 25000).

%%
%% OTP gen_server API
%%

init([]) ->
    process_flag(trap_exit, true),
    CT = ets:new(sut_table,   [ordered_set, private|?ETS_OPTS]),
    NT = ets:new(proc_table,      [ordered_set, named_table,
                                   public|?ETS_OPTS]),
    OT = ets:new(exception_table, [duplicate_bag, private|?ETS_OPTS]),
    {ok, #state{sut_table=CT, proc_table=NT, exception_table=OT}}.

handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(reset, _From, State=#state{sut_table=CT,
                                       proc_table=NT,
                                       exception_table=ET}) ->
    CPids = [CPid || {_, CPid} <- ets:tab2list(CT)],
    systest_cleaner:kill_wait(CPids, fun(P) -> erlang:exit(P, reset) end),
    kill_wait(find_procs(NT), infinity),
    [ets:delete_all_objects(T) || T <- [ET, NT, CT]],
    {reply, ok, State};
handle_call({force_stop, SutId, Timeout}, _From,
            State=#state{sut_table=CT}) ->
    log(framework, "~p force stop ~p within ~p ms~n",
        [?MODULE, SutId, Timeout]),
    case ets:lookup(CT, SutId) of
        [] ->
            log(framework,
                "~p not found in ~p~n", [SutId, ets:tab2list(CT)]),
            {reply, {error, regname, SutId}, State};
        [{SutId, Pid}=RefId] ->
            Reply = case systest_sut:stop(Pid, Timeout) of
                        {timeout, _} -> {timout, RefId};
                        Other        -> Other
                    end,
            log(framework, "force stop complete: ~p~n", [Reply]),
            {reply, Reply, State#state{last_timeout=Timeout}}
    end;
handle_call({exceptions, SutId}, _From,
            State=#state{exception_table=ET}) ->
    {reply, ets:match_object(ET, {SutId, '_', '_'}), State};
handle_call(clear_exceptions, _From, State=#state{exception_table=ET}) ->
    {reply, ets:delete_all_objects(ET), State};
handle_call({sut_started, SutId, SutPid},
            _From, State=#state{sut_table=CT}) ->
    case ets:insert_new(CT, {SutId, SutPid}) of
        true  -> %% NB: we link to sut pids so that we
                 %% can be sure to know when and why they exit
                 link(SutPid),
                 {reply, ok, State};
        false -> {reply, {error, clash}, State}
    end;
handle_call(dump, _From, State=#state{sut_table=CT,
                                      proc_table=NT,
                                      exception_table=OT}) ->
    [systest_utils:ets_dump(T) || T <- [CT, NT, OT]],
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason},
            State=#state{sut_table=CT, proc_table=NT,
                         exception_table=ET, last_timeout=Timeout}) ->
    case ets:match_object(CT, {'_', Pid}) of
        [{SutId, _}=Sut] ->
            log(framework,
                "watchdog handling sut (process) down"
                " event for ~p~n", [SutId]),

            case Reason of
                normal ->
                    %% the sut has exited normally - any procs left
                    %% alive are not supposed to be (they're orphans) and
                    %% so we report these as errors
                    Procs = find_procs(NT, Sut),
                    report_orphans(Sut, Procs, ET);
                noconfig ->
                    ok;
                _Other ->
                    %% the sut has crashed (reason /= normal)
                    ets:insert(ET, [{SutId, crashed, Reason}])
            end,

            handle_down(Sut, NT, Timeout),
            ets:delete(CT, SutId);
        [[]] ->
            log(framework,
                "sut down even unhandled: no id for "
                "~p in ~p~n", [Pid, ets:tab2list(CT)]),
            ok
    end,
    {noreply, State}.

terminate(_Reason, #state{proc_table=ProcTable}) ->
    kill_wait(find_procs(ProcTable), infinity),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

report_orphans(_, [], _) ->
    ok;
report_orphans({SutId, _}, Procs, ET) ->
    log(framework, "watchdog detected orphaned procs of dead sut ~p: ~p~n",
           [SutId, Procs]),
    ets:insert(ET, [{SutId, orphan, N} || N <- Procs, is_process_alive(N)]).

find_procs(ProcTable, {SutId, _}) ->
    [P || {{_, P}} <- ets:match_object(ProcTable, {{SutId, '_'}}),
          erlang:is_process_alive(P)].

find_procs(ProcTable) ->
    [P || {{_, P}} <- ets:tab2list(ProcTable), erlang:is_process_alive(P)].

handle_down(Sut, ProcTable, Timeout) ->
    kill_wait(find_procs(ProcTable, Sut), Timeout).

kill_wait([], _) ->
    log(framework, "watchdog: no procs to kill~n", []);
kill_wait(Procs, Timeout) ->
    log(framework, "watchdog killing: ~p~n", [Procs]),
    case systest_cleaner:kill_wait(Procs, fun systest_proc:kill/1, Timeout) of
        {error, timeout, _Dead} ->
            brutal_kill(Procs);
        {error, {killed, _Killed}} ->
            brutal_kill(Procs);
        {error, Reason} ->
            log(framework,
                "watching saw errors trying to kill children: ~p~n",
                [Reason]),
            brutal_kill(Procs);
        ok ->
            ok
    end.

brutal_kill(Targets) ->
    log(framework, "resorting to brutal_kill: ~p~n", [Targets]),
    [exit(P, kill) || P <- Targets].

