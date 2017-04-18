-module(sum_tests).
-include_lib("eunit/include/eunit.hrl").

bits_test_() ->
  [
   ?_assertEqual(3, sum:bits(7)),
   ?_assertEqual(1, sum:bits(8))
  ].
