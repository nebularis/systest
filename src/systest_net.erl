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
-module(systest_net).

-include_lib("kernel/include/inet.hrl").

-export([is_epmd_contactable/2]).

-define(DEFAULT_EPMD_PORT, 4369).

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

epmd_port() ->
    %% based on the gigantic assumption that every EPDM instance
    %% is running on the same port, but how could it be otherwise?
    case os:getenv("ERL_EPMD_PORT") of
        false -> ?DEFAULT_EPMD_PORT;
        PortNum -> list_to_integer(PortNum)
    end.

