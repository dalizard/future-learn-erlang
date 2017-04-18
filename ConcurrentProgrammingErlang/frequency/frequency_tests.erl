-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%
%%% TESTS DESCRIPTIONS
%%%
boot_up_test_() ->
  {"Can be started with a registered name",
   ?setup(fun boot_up/1)}.

shutdown_test_() ->
  {"Can be shutdown and unregistered",
   ?setup(fun shutdown/1)}.

allocation_test_() ->
  [{"Can allocate new frequencies",
    ?setup(fun allocation/1)},
   {"Returns an error when no frequencies are left",
    ?setup(fun no_frequencies_left/1)},
   {"Limits the number of frequencies per client to 1",
    ?setup(fun frequencies_limit_per_client/1)}].

deallocation_test_() ->
  [{"Can deallocate frequencies",
   ?setup(fun deallocation/1)},
   {"Returns an error when trying to deallocate a frequency not in use",
    ?setup(fun frequency_is_not_in_use/1)}].

%%%
%%% SETUP FUNCTIONS
%%%
start() ->
  frequency:start().

stop(Pid) ->
  Pid ! {request, self(), stop},
  timer:sleep(5).

%%%
%%% ACTUAL TESTS
%%%
boot_up(Pid) ->
  [
   ?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(frequency))
  ].

shutdown(Pid) ->
  Pid ! {request, self(), stop},
  [
   ?_assertNot(erlang:is_process_alive(Pid)),
   ?_assertEqual(undefined, whereis(frequency))
  ].

allocation(Pid) ->
  Response = allocate(Pid),
  ?_assertEqual(10, Response).

deallocation(Pid) ->
  allocate(Pid),
  Response = deallocate(Pid, 10),
  ?_assertEqual(ok, Response).

no_frequencies_left(Pid) ->
  [allocate_one_per_client(Pid) || _ <- lists:seq(1,6)],
  timer:sleep(5),
  Response = no_frequency(Pid),
  ?_assertEqual(ok, Response).

frequency_is_not_in_use(Pid) ->
  Response = deallocate_failure(Pid, 10),
  ?_assertEqual(ok, Response).

frequencies_limit_per_client(Pid) ->
  allocate(Pid),
  Response = allocate_another_frequency(Pid),
  ?_assertEqual(ok, Response).

%%%
%%% HELPER FUNCTIONS
%%%
allocate(Pid) ->
  Pid ! {request, self(), allocate},
  receive
    {reply, {ok, Frequency}} -> Frequency
  end.

deallocate(Pid, Frequency) ->
  Pid ! {request, self(), {deallocate, Frequency}},
  receive
    {reply, ok} -> ok
  end.

allocate_one_per_client(Pid) ->
  spawn(fun() -> Pid ! {request, self(), allocate} end).

no_frequency(Pid) ->
  Pid ! {request, self(), allocate},
  receive
    {reply, {error, no_frequency}} -> ok
  end.

deallocate_failure(Pid, Frequency) ->
  Pid ! {request, self(), {deallocate, Frequency}},
  receive
    {reply, {error, frequency_not_in_use}} -> ok
  end.

allocate_another_frequency(Pid) ->
  Pid ! {request, self(), allocate},
  receive
    {reply, {error, one_frequency_per_client}} -> ok
  end.
