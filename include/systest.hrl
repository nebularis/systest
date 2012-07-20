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
-define(CONFIG(Key, Conf), systest_config:read(Key, Conf)).
-define(CONFIG(Key, Conf, Default), systest_config:read(Key, Conf, Default)).
-define(REQUIRE(Key, Conf), systest_config:require(Key, Conf)).
-define(ECONFIG(Key, Conf), systest_config:eval(Key, Conf)).
-define(ENCONFIG(Key, Conf),
                systest_config:eval(Key, Conf,
                                    [{callback, {proc,
                                     fun systest_proc:get/2}}])).
-define(WRITE(Key, Value, Conf),
        lists:keyreplace(Key, 1, Conf, Value)).
-define(REPLACE(Key, Value, Conf),
        systest_config:replace_value(Key, Value, Conf)).
-define(ENV(A, B), systest_utils:lookup_env(A, B)).

-type application_info() :: {atom(), [{atom(), term()}]}.

-type command()          :: atom().
-type name()             :: string().
-type value()            :: string() | binary().
-type attribute()        :: atom().
-type setenv_flags()     :: {'environment', [{name(), value()}]} |
                            {'proc', attribute()}.
-type vmflags()          :: string().
-type program()          :: {'program', string()} |
                            {'args', [{name(), value()}]}.
-type script_flags()     :: [program() | value() | setenv_flags()].
-type hook()             :: script_flags()                      |
                            {module(), function(), [term()]}    |
                            {'local', module(), function(), [term()]}.

-record(execution, {
    base_dir        :: file:filename(),
    profile         :: systest_profile:profile(),
    targets  = []   :: systest_config:config(),
    options  = []   :: systest_config:config(),
    settings = []   :: systest_config:config(),
    base_config     :: systest_config:config(),
    quiet           :: boolean()
}).

%% TODO: deprecate the 'apps' field (merge into flags?)
-record(proc, {
    scope       :: atom(),                      %% usually the sut id...
    host        :: atom(),                      %% host name
    name        :: atom(),                      %% identifying (name)
    handler     :: module(),                    %% backing module
    link        :: boolean(),                   %% use start_link, or just start
    user        :: term(),                      %% user-defined data
    private     :: term(),                      %% handler data...
    %% TODO: this spec is *clearly* inadequate for the types we consume!
    flags       :: vmflags() | script_flags(),  %% used for 'all sorts',
                                                %% depending on the handler
    apps        :: [application_info()],        %% mainly used by slave
    on_start    :: hook(),                      %% startup hooks
    on_stop     :: hook(),                      %% shutdown hooks
    on_join     :: hook(),                      %% joining (sut) hooks
    id          :: atom(),                      %% set by the handler
    os_pid      :: string(),                    %% set by the handler
    owner       :: pid() | port(),              %% set by the handler
    cover       :: term(),                      %% use code coverage
    config      :: systest_config:config()
}).

-record(sut, {
    id              :: atom(),
    scope           :: atom(),
    name            :: atom(),
    procs           :: [pid()],
    config          :: systest_config:config(),
    on_start        :: [hook()],
    pending = []    :: [{term(), atom()}]
}).

