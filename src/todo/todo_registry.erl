%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(todo_registry).

-behaviour(gen_server).

-export([start_link/0, via_tuple/1, child_spec/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

% No local registry available in Erlang's stdlib.
via_tuple(Key) ->
  {via, global, {?MODULE, Key}}.

child_spec() ->
  #{
    id => ?MODULE,
    start => {?MODULE, start_link, []},
    type => supervisor
  }.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> {'ok', pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {'ok', #{}}.
init([]) ->
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, State, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(_, _) -> {'noreply', _}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> 'ok'.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, _, _) -> {'ok', _}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
