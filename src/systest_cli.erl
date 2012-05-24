%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com).
%%
%% Some portions of the code taken from sh (c) 2005 - 2012 Nebularis
%% Some portions of the code taken from rebar (c) 2010 Dave Smith
%% Some portions of the code taken from retest (c) 2010 Dave Smith
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
-module(systest_cli).

-behaviour(systest_node).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API Exports

-export([start/1, start_link/1, stop/1, kill/1]).
-export([status/1, interact/2]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% private record for tracking...
-record(sh, {pid, port, command, node}).

-include("systest.hrl").

-ifdef(TEST).
-export([convert_flags/3]).
-endif.

-spec start(systest_node:node_info()) -> systest_node:node_info() | term().
start(NI=#'systest.node_info'{host=Host, name=Name, flags=Flags}) ->
    NI.

-spec start_link(systest_node:node_info()) ->
                            systest_node:node_info() | term().
start_link(NI=#'systest.node_info'{host=Host, name=Name, flags=Flags}) ->
    NI.

-spec stop(systest_node:node_info()) -> 'ok'.
stop(#'systest.node_info'{id=Node}) ->
    slave:stop(Node).

-spec kill(systest_node:node_info()) -> 'ok'.
kill(Node) ->
    stop(Node).

-spec status(systest_node:node_info()) -> 'nodeup' | {'nodedown', term()}.
status(#'systest.node_info'{id=Node}) ->
    case net_adm:ping(Node) of
        pong  -> nodeup;
        Other -> {nodedown, Other}
    end.

-spec interact(systest_node:node_info(),
               {module(), atom(), [term()]}) -> term().
interact(#'systest.node_info'{id=Node}, {Mod, Func, Args}) ->
    rpc:call(Node, Mod, Func, Args).

%%
%% OTP gen_server API
%%

init([Cmd, Extra, Node]) ->
    case check_command(Cmd) of
        ok ->
            Env = ?CONFIG(env, Extra, []),
            ExecutableCommand = patch_command(Cmd, Env),
            Port = open_port({spawn, ExecutableCommand},
                             [exit_status, {line, 16384},
                              use_stdio, stderr_to_stdout]),
            Pid = read_pid(Port),
            {ok, #sh{pid=Pid, port=Port,
                     command=ExecutableCommand, node=Node}};
        StopError ->
            StopError
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

%% port handling

read_pid(Port) ->
    receive
        {Port, {data, {eol, Pid}}} ->
            Pid;
        {Port, {exit_status, Rc}} ->
            {error, {stopped, Rc}}
    end.

%% command processing

check_command(Cmd) ->
    case re:run(Cmd, "(&&|;)") of
        nomatch -> ok;
        _       -> {stop, async_multicmds_disallowed}
    end.

patch_command(Cmd, Env) ->
    case os:type() of
        {win32, _} ->
            "cmd /q /c " ++ lists:foldl(fun({Key, Value}, Acc) ->
                                            expand_env_variable(Acc, Key, Value)
                                        end, Cmd, Env);
        _ ->
            "/usr/bin/env sh -c \"echo $$; exec " ++ Cmd ++ "\""
    end.

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", [global]),
            %% Use a regex to match/replace:
            %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
            RegEx = io_lib:format("\\\$(~s(\\s|$)|{~s})", [VarName, VarName]),
            ReOpts = [global, {return, list}],
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

%% node configuration/setup

convert_flags(Node, Flags, Config) ->
    {_, _, Acc} = lists:foldl(fun process/2,
                              {Node, Config, [hd(Flags)]}, tl(Flags)),
    Acc.

process({node, Attr}, {Node, _, Output}=Acc) ->
    setelement(3, Acc, Output ++
                       [atom_to_list(systest_node:get_node_info(Attr, Node))]);
process({environment, Attr}, {_, Config, Output}=Acc) ->
    Value = ?REQUIRE(string:to_lower(Attr), Config),
    Env = Attr ++ "=" ++ Value,
    setelement(3, Acc, [Env|Output]);
process({environment, Attr, Value}, {_, _, Output}=Acc) ->
    Env = Attr ++ "=" ++ Value,
    setelement(3, Acc, [Env|Output]);
process(Data, {_, _, Output}=Acc) ->
    setelement(3, Acc, Output ++ [Data]).
