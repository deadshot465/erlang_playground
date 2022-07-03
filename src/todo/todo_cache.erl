%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_cache).

-behaviour(supervisor).

-export([start_link/0, server_process/1, child_spec/0]).
-export([init/1]).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> any().
start_link() ->
  io:format("~s~n", ["Starting To-Do cache..."]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec server_process(_) -> any().
server_process(ToDoListName) ->
  case supervisor:start_child(?MODULE, [ToDoListName]) of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> Pid
  end.

child_spec() ->
  #{
    id => ?MODULE,
    start => {?MODULE, start_link, []},
    type => supervisor
  }.

-spec init(_) -> {'ok', #{}}.
init(_) ->
  SupFlags = #{
    strategy => simple_one_for_one
  },
  {ok, {SupFlags, [todo_server:child_spec()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
