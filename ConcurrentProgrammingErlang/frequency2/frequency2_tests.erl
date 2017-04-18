-module(frequency2_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%
%%% TESTS DESCRIPTIONS
%%%
start_test_() ->
  {"Can be started with a registered name",
   ?setup(fun start_up/1)}.

stop_test_() ->
  {"Can be stopped",
   ?setup(fun shutdown/1)}.

allocation_test_() ->
  {"Can allocate frequencies",
   ?setup(fun allocation/1)}.

deallocation_test_() ->
  {"Can deallocate frequencies",
   ?setup(fun deallocation/1)}.

flushing_mailbox_test_() ->
  {"Can flush the mailbox of unread messages",
   ?setup(fun flush_mailbox/1)}.

server_busy_test_() ->
  {"Can timeout when allocation takes too long",
   ?setup(fun server_busy/1)}.

%%%
%%% ACTUAL TESTS
%%%
start_up(_) ->
  ?_assert(erlang:is_process_alive(whereis(frequency))).

shutdown(_) ->
  frequency2:stop(),
  ?_assertEqual(undefined, whereis(frequency)).

allocation(_) ->
  Result = frequency2:allocate(),
  ?_assertEqual({ok, 10}, Result).

deallocation(_) ->
  frequency2:allocate(),
  Result = frequency2:deallocate(10),
  ?_assertEqual(ok, Result).

flush_mailbox(_) ->
  frequency ! an_unexpected_message,
  frequency2:allocate(),
  Result = erlang:process_info(whereis(frequency), messages),
  ?_assertEqual({messages, []}, Result).

server_busy(_) ->
  erlang:suspend_process(whereis(frequency)),
  Result = frequency2:allocate(),
  erlang:resume_process(whereis(frequency)),
  ?_assertEqual(timeout, Result).

%%%
%%% SETUP FUNCTIONS
%%%
start() ->
  frequency2:start(),
  timer:sleep(5).

stop(_) ->
  catch frequency2:stop().
