%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_database_worker).

-behaviour(gen_server).

-export([start_link/1, start/1, store/3, get/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec store(integer(), _, _) -> 'ok'.
store(WorkerId, Key, Data) -> gen_server:cast(via_tuple(WorkerId), {store, Key, Data}).

-spec get(integer(), _) -> any().
get(WorkerId, Key) -> gen_server:call(via_tuple(WorkerId), {get, Key}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start({string(), integer()}) -> 'ignore' | {'error', _} | {'ok', pid()}.
start({Folder, WorkerId}) ->
  gen_server:start(via_tuple(WorkerId), ?MODULE, Folder, []).

-spec start_link({string(), integer()}) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link({Folder, WorkerId}) ->
  io:format("~s~n", ["Starting database worker..."]),
  gen_server:start_link(via_tuple(WorkerId), ?MODULE, Folder, []).

-spec init(_) -> {'ok', _}.
init(Folder) ->
  {ok, Folder}.

-spec handle_call({'get', string()}, pid(), _) -> {'noreply', _}.
handle_call({get, Key}, From, Folder) ->
  spawn(fun() ->
    FilePath = file_name(Key, Folder),
    Data = case file:read_file(FilePath) of
             {ok, Binary} -> binary_to_term(Binary);
             _ -> false
           end,
    gen_server:reply(From, Data)
        end),
  {noreply, Folder}.

-spec handle_cast({'store', _, _}, _) -> {'noreply', _}.
handle_cast({store, Key, Data}, Folder) ->
  spawn(fun() ->
    FilePath = file_name(Key, Folder),
    file:write_file(FilePath, term_to_binary(Data))
        end),
  {noreply, Folder}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec file_name(string(), string()) -> string().
file_name(Key, Folder) ->
  Folder ++ "/" ++ Key.

via_tuple(WorkerId) ->
  todo_registry:via_tuple({?MODULE, WorkerId}).