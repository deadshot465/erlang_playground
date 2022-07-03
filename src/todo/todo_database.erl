%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_database).

-behaviour(gen_server).

-export([start_link/0, start/0, store/2, get/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(FOLDER, "./persist").

-spec store(string(), any()) -> any().
store(Key, Data) ->
  WorkerPid = gen_server:call(?SERVER, {choose_worker, Key}),
  todo_database_worker:store(WorkerPid, Key, Data).

-spec get(string()) -> any().
get(Key) ->
  WorkerPid = gen_server:call(?SERVER, {choose_worker, Key}),
  todo_database_worker:get(WorkerPid, Key).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start() -> 'ignore' | {'error', _} | {'ok', pid()}.
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
  io:format("~s~n", ["Starting database server..."]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init(_) -> {'ok', _}.
init([]) ->
  file:make_dir(?FOLDER),
  Workers = lists:map(fun(Index) ->
    {ok, Pid} = todo_database_worker:start_link(?FOLDER),
    {Index, Pid}
                          end, lists:seq(0, 2)),
  {ok, maps:from_list(Workers)}.

-spec handle_call({'choose_worker', _}, _, map()) -> {'reply', pid(), map()}.
handle_call({choose_worker, Key}, _From, Workers) ->
  {reply, choose_worker(Key, Workers), Workers}.

-spec handle_cast(_, _) -> {'noreply', _}.
handle_cast(_Request, Workers) ->
  {noreply, Workers}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec choose_worker(string(), map()) -> pid().
choose_worker(Key, Workers) ->
  Hash = erlang:phash2(Key, 3),
  maps:get(Hash, Workers).