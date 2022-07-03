%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_database).

-behaviour(supervisor).

-export([start_link/0, store/2, get/1, child_spec/0]).
-export([init/1]).

-define(FOLDER, "./persist").
-define(POOL_SIZE, 3).

-spec store(string(), any()) -> any().
store(Key, Data) ->
  WorkerId = choose_worker(Key),
  todo_database_worker:store(WorkerId, Key, Data).

-spec get(string()) -> any().
get(Key) ->
  WorkerId = choose_worker(Key),
  todo_database_worker:get(WorkerId, Key).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
  io:format("~s~n", ["Starting database server supervisor..."]),
  file:make_dir(?FOLDER),
  supervisor:start_link(?MODULE, []).

init(_) ->
  SupFlags = #{
    strategy => one_for_one
  },
  Children = lists:map(fun(Index) -> worker_spec(Index) end, lists:seq(1, ?POOL_SIZE)),
  {ok, {SupFlags, Children}}.

child_spec() ->
  #{
    id => ?MODULE,
    start => {?MODULE, start_link, []},
    type => supervisor
  }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

choose_worker(Key) ->
  erlang:phash2(Key, 3) + 1.

worker_spec(WorkerId) ->
  #{
    id => WorkerId,
    start => {todo_database_worker, start_link, [{?FOLDER, WorkerId}]}
  }.