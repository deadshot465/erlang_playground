%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2022 6:35
%%%-------------------------------------------------------------------
-module(todo_system).
-author("chehui.chou").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 5,
    period => 10
  },
  ChildSpecs = [
    todo_registry:child_spec(),
    todo_database:child_spec(),
    todo_cache:child_spec()
  ],
  {ok, {SupFlags, ChildSpecs}}.