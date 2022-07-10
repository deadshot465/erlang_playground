%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(simple_registry).

-behaviour(gen_server).

-export([start_link/0, whereis/1, register/1, handle_call/3, handle_cast/2]).
-export([init/1, handle_info/2]).

-define(SERVER, ?MODULE).

whereis(Key) ->
  case ets:lookup(?SERVER, Key) of
    [{Key, Value}] -> Value;
    _ -> nil
  end.

register(Key) ->
  link(erlang:whereis(?SERVER)),
  case ets:insert_new(?SERVER, {Key, self()}) of
    true -> ok;
    _ -> error
  end.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?SERVER, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, nil}.

handle_call(_Request, _From, State) ->
  {reply, nil, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
  ets:match_delete(?SERVER, {'_', Pid}),
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================