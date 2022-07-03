-module(library).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([borrow_book/2, return_book/2, close/1, start/0, start_link/0]).

-spec start() -> any().
start() ->
  {ok, Pid} = gen_server:start(?MODULE, [<<"AWS JumpStart for NewGrads 2022"/utf8>>, <<"Getting Started with C# 10.0"/utf8>>], []),
  Pid.

-spec start_link() -> any().
start_link() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [<<"AWS JumpStart for NewGrads 2022"/utf8>>, <<"Getting Started with C# 10.0"/utf8>>], []),
  Pid.

-spec init(any()) -> tuple().
init(Args) ->
  {ok, Args}.

-spec borrow_book(pid(), unicode:charlist()) -> any().
borrow_book(Pid, BookName) ->
  gen_server:call(Pid, {borrow, BookName}).

-spec return_book(pid(), unicode:charlist()) -> any().
return_book(Pid, BookName) ->
  gen_server:cast(Pid, {return, BookName}).

-spec close(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> any().
close(Pid) ->
  gen_server:call(Pid, terminate).

-spec handle_call(tuple(), any(), any()) -> any().
handle_call({borrow, BookName}, _From, State) ->
  case lists:member(BookName, State) of
    true -> {reply, BookName, State -- [BookName]};
    _ ->
      io:format("There is no such book named ~ts in the library!~n", [BookName]),
      {reply, no_such_book, State}
  end;
handle_call(terminate, _From, State) ->
  {stop, normal, State}.

-spec handle_cast({'return', _}, [any()]) -> {'noreply', [any()]}.
handle_cast({return, BookName}, State) ->
  case lists:member(BookName, State) of
    false ->
      io:format("Returned the book ~ts~n", [BookName]),
      {noreply, State ++ [BookName]};
    _ ->
      io:format("There is already a book with the same name in the library!~n"),
      {noreply, State}
  end.

-spec terminate('normal', [any()]) -> 'ok'.
terminate(normal, State) ->
  lists:foreach(fun(S) ->
    io:format("~ts is burnt.~n", [S])
    end, State),
  ok.