%% Copyright (c) 2012 Nebularis.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(systest_assert).

-export([throw_unless/2, throw_unless/3, throw_unless/4]).

throw_unless(Cond, Msg) ->
    throw_unless(Cond, Msg, []).

throw_unless(Cond, Msg, FmtArgs) ->
    throw_unless(Cond, system, Msg, FmtArgs).

throw_unless(Cond, SubSys, Msg, FmtArgs) ->
    case Cond of
        true ->
            ok;
        false ->
            systest_log:log(SubSys, Msg, FmtArgs),
            throw({SubSys, assertion_failed})
    end.
