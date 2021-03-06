%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_database).

-behaviour(supervisor).

-export([store/2, get/1, child_spec/0, store_local/2]).
-export([init/1]).

-define(FOLDER, "./persist").
-define(POOL_SIZE, 3).

store(Key, Data) ->
  {_Results, BadNodes} = rpc:multicall(?MODULE, store_local, [Key, Data], timer:seconds(5)),
  lists:foreach(fun(Node) ->
    Message = io_lib:format("Store failed on node ~p.~n", [Node]),
    io:format("~s", [lists:flatten(Message)])
                end, BadNodes),
  ok.

-spec store_local(string(), any()) -> any().
store_local(Key, Data) ->
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
  NodeIdentifier = hd(string:tokens(erlang:atom_to_list(erlang:node()), "@")),
  FolderName = ?FOLDER ++ "/" ++ NodeIdentifier,
  io:format("~s~n", ["Starting database server supervisor using poolboy..."]),
  file:make_dir(FolderName),
  poolboy:child_spec(?MODULE, [
    {name, {local, ?MODULE}},
    {worker_module, todo_database_worker},
    {size, 3}
  ], [FolderName]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

worker_spec(WorkerId) ->
  #{
    id => WorkerId,
    start => {todo_database_worker, start_link, [{?FOLDER, WorkerId}]}
  }.