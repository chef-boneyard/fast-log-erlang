%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode Inc.

-module(fast_log_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, I, Type, Config), {Name, {I, start_link, Config}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = build_children(application:get_env(fast_log, loggers)),
    {ok, { {one_for_one, 5, 10}, Children} }.

%% Internal functions
build_children({ok, Loggers}) ->
    build_children(Loggers, []);
build_children(_) ->
    [].

build_children([], Children) ->
    Children;
build_children([H|T], Children) ->
    Name = proplists:get_value(name, H),
    Child = ?CHILD(Name, fast_log, worker, [H]),
    build_children(T, [Child|Children]).
