%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0, init/0]).

start() ->
  Pid = spawn(?MODULE, init, []),
  register(?MODULE, Pid),
  Pid.

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {Reply, NewFrequencies} = deallocate(Frequencies, Freq),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped},
      unregister(frequency)
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keymember(Pid, 2, Allocated) of
    true ->
      {{Free, Allocated}, {error, one_frequency_per_client}};
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq) ->
  case lists:keyfind(Freq, 1, Allocated) of
    false ->
      {{error, frequency_not_in_use}, {Free, Allocated}};
    _ ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {ok, {[Freq|Free],  NewAllocated}}
  end.
