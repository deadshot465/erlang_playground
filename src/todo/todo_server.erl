%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2022 17:53
%%%-------------------------------------------------------------------
-module(todo_server).
-author("chehui.chou").

-behaviour(gen_server).
-include("todo_list_record.hrl").

%% API
-export([start_link/1, start/1, add_entry/2, list_entries/1, delete_entry/2, update_entry/2, child_spec/0, child_spec/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec start(string()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start(ToDoListName) ->
  io:format("Starting server for ~s~n...", [ToDoListName]),
  gen_server:start(via_tuple(ToDoListName), ?MODULE, ToDoListName, []).

%% @doc Spawns the server and registers the local name (unique)
-spec start_link(string()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(ToDoListName) ->
  io:format("Starting server for ~s~n...", [ToDoListName]),
  gen_server:start_link(via_tuple(ToDoListName), ?MODULE, ToDoListName, []).

-spec add_entry(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> 'ok'.
add_entry(Pid, Entry) -> gen_server:cast(Pid, {add, Entry}).

-spec delete_entry(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> 'ok'.
delete_entry(Pid, Key) -> gen_server:cast(Pid, {delete, Key}).

-spec update_entry(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> 'ok'.
update_entry(Pid, Entry) -> gen_server:cast(Pid, {update, Entry}).

-spec list_entries(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> any().
list_entries(Pid) -> gen_server:call(Pid, list).

child_spec() ->
  #{
    id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => temporary
  }.

child_spec(ToDoListName) ->
  #{
    id => ?MODULE,
    start => {?MODULE, start_link, [ToDoListName]},
    restart => temporary
  }.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(_) -> {'ok', 'false'}.
init(ToDoListName) ->
  self() ! {real_init, ToDoListName},
  {ok, false}.

%% @private
%% @doc Handling call messages
-spec handle_call('list', _, {_, _}) -> {'reply', _, {_, _}}.
handle_call(list, _From, State = {_, ToDoList}) ->
  {reply, todo_list:list_entries(ToDoList), State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast({'add', _} | {'delete', _} | {'update', _}, {_, _}) -> {'noreply', {_, _}}.
handle_cast({add, Entry}, {ToDoListName, OldToDoList}) ->
  NewState = todo_list:add_entry(OldToDoList, Entry),
  todo_database:store(ToDoListName, NewState),
  {noreply, {ToDoListName, NewState}};

handle_cast({delete, Key}, {ToDoListName, OldToDoList}) ->
  NewState = todo_list:delete_entry(OldToDoList, Key),
  todo_database:store(ToDoListName, NewState),
  {noreply, {ToDoListName, NewState}};

handle_cast({update, Entry}, {ToDoListName, OldToDoList}) ->
  NewState = todo_list:update_entry(OldToDoList, Entry),
  todo_database:store(ToDoListName, NewState),
  {noreply, {ToDoListName, NewState}}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info({'real_init', _}, _) -> {'noreply', {_, _}}.
handle_info({real_init, ToDoListName}, _) ->
  Result = case todo_database:get(ToDoListName) of
             false -> todo_list:new();
             X -> X
           end,
  {noreply, {ToDoListName, Result}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

via_tuple(ToDoListName) ->
  todo_registry:via_tuple({?MODULE, ToDoListName}).