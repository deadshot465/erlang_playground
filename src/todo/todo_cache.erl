%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_cache).

-behaviour(gen_server).

-export([start_link/1, start/0, server_process/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start() -> 'ignore' | {'error', _} | {'ok', pid()}.
start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(_) -> any().
start_link(_) ->
  io:format("~s~n", ["Starting To-Do cache..."]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec server_process(_) -> any().
server_process(ToDoListName) -> gen_server:call(?MODULE, {server_process, ToDoListName}).

-spec init([]) -> {'ok', #{}}.
init([]) ->
  todo_database:start_link(),
  {ok, #{}}.

-spec handle_call({'server_process', _}, _, map()) -> {'reply', _, map()}.
handle_call({server_process, ToDoListName}, _From, State) ->
  case maps:find(ToDoListName, State) of
    {ok, Value} -> {reply, Value, State};
    _ ->
      {ok, NewServer} = todo_server:start_link(ToDoListName),
      {reply, NewServer, State#{ToDoListName => NewServer}}
  end.

-spec handle_cast(_, _) -> {'noreply', _}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
