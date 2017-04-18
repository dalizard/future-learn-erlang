-module(palin).
-export([server/1, loop/1, palin/1, nopunct/1, palindrome/1]).

server(To) ->
  register(palin, spawn(?MODULE, loop, [To])).

loop(To) ->
  receive
    {check, String} ->
      case palindrome(String) of
        true -> To ! piss_off_mate;
        false -> To ! go_to_hell
      end,
      loop(To);
    _ ->
      {ok, stopped}
  end.

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
  palin(nocaps(nopunct(Xs))).

nopunct([]) ->
  [];
nopunct([X|Xs]) ->
  case lists:member(X,".,\ ;:\t\n\'\"") of
    true ->
      nopunct(Xs);
    false ->
      [ X | nopunct(Xs) ]
  end.

nocaps([]) ->
  [];
nocaps([X|Xs]) ->
  [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
  case $A =< X andalso X =< $Z of
    true ->
      X+32;
    false ->
      X
  end.

% literal palindrome

palin(Xs) ->
  Xs == reverse(Xs).

reverse(Xs) ->
  shunt(Xs,[]).

shunt([],Ys) ->
  Ys;
shunt([X|Xs],Ys) ->
  shunt(Xs,[X|Ys]).





