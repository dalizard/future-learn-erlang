-module(index_tests).
-include_lib("eunit/include/eunit.hrl").

index_text_test() ->
  [
   ?assertEqual([{"foobar", [{4, 4}, {6, 6}]}, {"text", [{1, 3}, {5, 5}, {7, 8}]}],
                index:process(["Text", "text text", "text", "foobar", "text", "foobar",
                            "text", "text"])),
   ?assertEqual([{"cranky",    [{5,5}]},
                 {"hanky",     [{2,2}]},
                 {"mankey",    [{1,1}]},
                 {"never",     [{2,2}]},
                 {"pants",     [{4,4}]},
                 {"plants",    [{3,3}]},
                 {"quite",     [{5,5}]},
                 {"soil",      [{4,4}]},
                 {"sometimes", [{5,5}]},
                 {"when",      [{1,1}]}],
                index:process(["When I'm old and mankey,", "I'll never use a hanky",
                               "I'll wee on plants", "and soil my pants",
                               "and sometimes get quite cranky."]))
  ].

words_filter_test_() ->
  [
   ?_assertEqual([], index:words_filter([])),
   ?_assertEqual([], index:words_filter(["the", "a"])),
   ?_assertEqual(["When", "mankey", "never", "hanky"],
                 index:words_filter(["When", "I'm", "old", "and", "mankey,",
                                     "I'll", "never", "use", "a", "hanky!"]))
  ].
