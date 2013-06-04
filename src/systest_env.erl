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
%% ----------------------------------------------------------------------------
-module(systest_env).

-export([default_log_dir/1, default_scratch_dir/0, temp_dir/0]).
-export([is_epmd_contactable/2, hostname/1]).
-export([work_directory/3, lookup_env/2, timestamp/0]).
-export([is_exported/3]).

-define(DEFAULT_EPMD_PORT, 4369).

-include_lib("kernel/include/inet.hrl").
-include("systest.hrl").

is_exported(M, F, A) ->
    code:ensure_loaded(M),
    erlang:function_exported(M, F, A).

default_log_dir(Config) ->
    ?CONFIG(log_dir, Config, filename:join(default_scratch_dir(), "logs")).

default_scratch_dir() ->
    work_directory(temp_dir(), systest, "SYSTEST_SCRATCH_DIR").

hostname(NodeName) ->
    element(2, systest_utils:proc_id_and_hostname(NodeName)).

temp_dir() ->
    %% TODO: move this into hyperthunk/rebar_plugin_manager?
    case os:type() of
        {win32, _} ->
            %% mirrors the behaviour of the win32 GetTempPath function...
            get("TMP", get("TEMP", element(2, file:get_cwd())));
        {unix, darwin} ->
            "/tmp";  %% otherwise TMPDIR changes and throws off ct log collation
        _ ->
            case os:getenv("TMPDIR") of
                false -> "/tmp"; %% this is what the JVM does, but honestly...
                Dir   -> Dir
            end
    end.

work_directory(Base, Name, EnvOverride) ->
    lookup_env(EnvOverride, filename:join(Base, Name)).

lookup_env(EnvKey, Default) ->
    case os:getenv(EnvKey) of
        false -> Default;
        Value -> Value
    end.

timestamp() ->
    Now = now(),
    {{Year,Month,Day}, {Hour,Min,Sec}} = calendar:now_to_local_time(Now),
    string:join([lists:flatten(io_lib:format("~4.10.0B", [Year]))|
                [begin
                     lists:flatten(io_lib:format("~2.10.0B", [X]))
                 end || X <- [Day,Month,Hour,Min,Sec]]], "-").

%%
%% @doc returns the atom 'true' if epmd running on Host is visible
%% from the calling node, otherwise {false, Reason::term()}.
%%
-spec is_epmd_contactable(Host::atom(),
                          Timeout::integer()) -> 'true' | {'false', term()}.
is_epmd_contactable(Host, Timeout) ->
    case inet:gethostbyname(Host, inet, Timeout) of
        {ok, #hostent{h_name=H_Name}} ->
            %% is host reachable?
            case gen_tcp:connect(H_Name, epmd_port(), [inet], Timeout) of
                {error, Reason} ->
                    {false, Reason};
                {ok, Sock} ->
                    ok = gen_tcp:close(Sock),
                    true
            end;
        {error, Reason} ->
            {false, {dns, Reason}}
    end.

get(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Value
    end.

epmd_port() ->
    %% based on the gigantic assumption that every EPDM instance
    %% is running on the same port, but how could it be otherwise?
    case os:getenv("ERL_EPMD_PORT") of
        false -> ?DEFAULT_EPMD_PORT;
        PortNum -> list_to_integer(PortNum)
    end.

