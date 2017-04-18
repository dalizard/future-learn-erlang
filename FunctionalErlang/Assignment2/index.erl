-module(index).
-export([get_file_contents/1,show_file_contents/1,
         words_filter/1, process/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
           Partial;
    Line -> {Strip,_} = lists:split(length(Line)-1,Line),
            get_all_lines(File,[Strip|Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.

process(Lines) ->
  Index = index_lines(Lines),
  SortedIndex = lists:sort(Index),
  group_line_numbers(SortedIndex).

index_lines(Lines) ->
  index_lines(Lines, 1, []).

index_lines([], _, Index) ->
  Index;
index_lines([L|Ls], LineNumber, Index) ->
  Words = words_filter(split_line(string:to_lower(L))),
  index_lines(Ls, LineNumber + 1, index_words(LineNumber, Words, Index)).

index_words(_, [], Index) ->
  Index;
index_words(LineNumber, [W | Ws], Index) ->
  index_words(LineNumber, Ws, update_index(Index, W, LineNumber)).

group_line_numbers([]) ->
  [];
group_line_numbers([{Word, LineNumbers}| Words]) ->
  Grouped = {Word, group_numbers(LineNumbers)},
  [Grouped | group_line_numbers(Words)].

update_index([], Word, Line) ->
  [{Word, [Line]}];
update_index([{Word, Lines} | Is], Word, Line) ->
  [{Word, insert_num_if_not_exists(Line, Lines)} | Is];
update_index([I| Is], Word, Line) ->
  [I | update_index(Is, Word, Line)].

group_numbers([]) ->
  [];
group_numbers([N|Ns]) ->
  lists:reverse(group_numbers(Ns, {N,N}, [])).

group_numbers([], Group, Groups) ->
  [Group|Groups];
group_numbers([N|Ns], {S, E}, Groups) when E + 1 == N ->
  group_numbers(Ns, {S, N}, Groups);
group_numbers([N|Ns], {S, E}, Groups) ->
  group_numbers(Ns, {N, N}, [{S, E}| Groups]).

split_line(Line) ->
  split_line(Line, [], []).

split_line([], [], Words) ->
  Words;
split_line([], Word, Words) ->
  [lists:reverse(Word) | Words];
split_line([L | Ls], Word, Words) when L >= $A, L =< $z ->
  split_line(Ls, [L | Word], Words);
split_line([_L | Ls], Word, Words) ->
  case Word of
    [] ->
      split_line(Ls, [], Words);
    _ ->
      split_line(Ls, [], [lists:reverse(Word)|Words])
  end.

insert_num_if_not_exists(N, []) ->
  [N];
insert_num_if_not_exists(L, [L|_Ls] = LineNumbers) ->
  LineNumbers;
insert_num_if_not_exists(N, [L|Ls] = LineNumbers) ->
  case N > L of
    true ->
      [L | insert_num_if_not_exists(N, Ls)];
    false ->
      [N | LineNumbers]
  end.

words_filter(N) ->
  Tokens = lists:flatmap(fun(E) -> string:tokens(E, " ") end, N),
  SanitizedTokens = lists:map(fun(E) ->
                                  re:replace(E, "[^A-Za-z]", "", [global, {return, list}])
                              end, Tokens),
  lists:filter(fun(E) -> (length(E) > 3) and (string:str(E, "'") =:= 0) end, SanitizedTokens).
