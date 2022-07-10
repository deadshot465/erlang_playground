%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_database).

-behaviour(supervisor).

-export([store/2, get/1, child_spec/0]).
-export([init/1]).

-define(FOLDER, "./persist").
-define(POOL_SIZE, 3).

-spec store(string(), any()) -> any().
store(Key, Data) ->
  poolboy:transaction(?MODULE, fun(WorkerPid) -> todo_database_worker:store(WorkerPid, Key, Data) end).

-spec get(string()) -> any().
get(Key) ->
  poolboy:transaction(?MODULE, fun(WorkerPid) -> todo_database_worker:get(WorkerPid, Key) end).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init(_) ->
  SupFlags = #{
    strategy => one_for_one
  },
  Children = lists:map(fun(Index) -> worker_spec(Index) end, lists:seq(1, ?POOL_SIZE)),
  {ok, {SupFlags, Children}}.

child_spec() ->
  io:format("~s~n", ["Starting database server supervisor using poolboy..."]),
  file:make_dir(?FOLDER),
  poolboy:child_spec(?MODULE, [
    {name, {local, ?MODULE}},
    {worker_module, todo_database_worker},
    {size, 3}
  ], [?FOLDER]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

worker_spec(WorkerId) ->
  #{
    id => WorkerId,
    start => {todo_database_worker, start_link, [{?FOLDER, WorkerId}]}
  }.