-module(shapes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("shapes.hrl").

perimeter_test_() ->
  [
   ?_assertEqual(12, shapes:perimeter(#square{a = 3})),
   ?_assertEqual(31.41592653589793, shapes:perimeter(#circle{r = 5})),
   ?_assertEqual(40.6, shapes:perimeter(#rectangle{a = 8.3, b = 12})),
   ?_assertEqual(22, shapes:perimeter(#triangle{a = 7, b = 10, c = 5}))
  ].

area_test_() ->
  [
   ?_assertEqual(9.0, shapes:area(#square{a = 3})),
   ?_assertEqual(78.53981633974483, shapes:area(#circle{r = 5})),
   ?_assertEqual(99.60000000000001, shapes:area(#rectangle{a = 8.3, b = 12})),
   ?_assertEqual(16.24807680927192, shapes:area(#triangle{a = 7, b = 10, c = 5}))
  ].

enclose_test_() ->
  [
   ?_assertEqual(#rectangle{a = 3, b = 3}, shapes:enclose(#square{a = 3})),
   ?_assertEqual(#rectangle{a = 10.4, b = 10.4}, shapes:enclose(#circle{r = 5.2})),
   ?_assertEqual(#rectangle{a = 8.3, b = 12}, shapes:enclose(#rectangle{a = 8.3, b = 12})),
   ?_assertEqual(#rectangle{a = 3, b = 4}, shapes:enclose(#triangle{a = 3, b = 4, c = 5}))
  ].
