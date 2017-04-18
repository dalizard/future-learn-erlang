-module(sum).
-export([bits/1]).

bits(N) ->
  bits(N, 0).

bits(0, Acc) -> Acc;
bits(N, Acc) ->
  bits(N div 2, Acc + (N rem 2)).
