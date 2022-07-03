-module(test).
-export([infinite/1, take/2, filter/2, nth_prime/1, is_prime/1, factors/1, calculate_factors/3]).

-spec infinite(_) -> nonempty_improper_list(any(), fun(() -> no_return())).
infinite(N) -> [N | fun() -> infinite(N + 1) end].

-spec take(non_neg_integer(), _) -> [any()].
take(0, _) -> [];
take(N, [Head | Tail]) -> [Head | take(N - 1, Tail())].

-spec filter(fun((_) -> any()), nonempty_maybe_improper_list()) -> nonempty_improper_list(any(), fun(() -> nonempty_improper_list(any(), fun(() -> any())))).
filter(Fn, [Head | Tail]) ->
    case Fn(Head) of
        true -> [Head | fun() -> filter(Fn, Tail()) end];
        _ -> filter(Fn, Tail())
    end.

-spec prime(_, _, [any()]) -> [any()].
prime(Count, Dest, Acc) when Count =:= Dest -> Acc;
prime(Count, Dest, Acc) ->
    IsEmptyAcc = Acc =:= [],
    Base = if IsEmptyAcc -> 2;
            true -> hd(Acc)
           end,
    Prime = take(1, filter(fun(X) -> not (lists:any(fun(Y) -> math:fmod(X, Y) == 0 end, Acc)) end, infinite(Base))),
    prime(Count + 1, Dest, [hd(Prime) | Acc]).

-spec nth_prime(_) -> any().
nth_prime(N) ->
    Primes = prime(0, N, []),
    hd(Primes).

-spec is_prime(_) -> boolean().
is_prime(N) when N =< 1 -> false;
is_prime(2) -> true;
is_prime(N) ->
  Numbers = lists:seq(2, N - 1),
  lists:all(fun(X) -> N rem X =/= 0 end, Numbers).

factors(Value) -> calculate_factors(Value, 2, []).

calculate_factors(1, _, Acc) -> lists:reverse(Acc);
calculate_factors(CurrentValue, Base, Acc) ->
  case CurrentValue rem Base of
    0 -> calculate_factors(CurrentValue div Base, Base, [Base | Acc]);
    _ ->
        NextBase = case Base of
            2 -> 3;
            _ -> Base + 2
        end,
        calculate_factors(CurrentValue, NextBase, Acc)
  end.