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

-spec store(pid(), _, _) -> 'ok'.
store(WorkerPid, Key, Data) -> gen_server:call(WorkerPid, {store, Key, Data}).

-spec get(pid(), _) -> any().
get(WorkerPid, Key) -> gen_server:call(WorkerPid, {get, Key}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start({string(), integer()}) -> 'ignore' | {'error', _} | {'ok', pid()}.
start(Folder) ->
  gen_server:start(?MODULE, Folder, []).

-spec start_link({string(), integer()}) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Folder) ->
  io:format("~s~n", ["Starting database worker..."]),
  gen_server:start_link(?MODULE, Folder, []).

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
  {noreply, Folder};

handle_call({store, Key, Data}, From, Folder) ->
  spawn(fun() ->
    FilePath = file_name(Key, Folder),
    file:write_file(FilePath, term_to_binary(Data)),
    gen_server:reply(From, ok)
        end),
  {noreply, Folder}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec file_name(string(), string()) -> string().
file_name(Key, Folder) ->
  Folder ++ "/" ++ Key.