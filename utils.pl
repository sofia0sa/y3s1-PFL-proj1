:- use_module(library(lists)).
% get_name(+Player)
% Asks player name. Dynamically adds the name_of/2 fact to the base fact
get_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    read_line(Name),
    asserta(name_of(Player, Name)).

print_heart :-
    write('  /\\  /\\  '), nl,
    write(' /  \\/  \\ '), nl,
    write(' \\      / '), nl,
    write('  \\    /  '), nl,
    write('   \\  /   '), nl,
    write('    \\/    ').

chess_pieces :-
    write(' K  Q  R  B  N  P'), nl,
    write(' k  q  r  b  n  p').
    
% init_random_state/0
% Initialize the random module
init_random_state :-
    now(X),
    setrand(X).

% get_option(+Min,+Max,+Context,-Value)
% Unifies Value with the value given by user input between Min and Max when asked about Context
choose_number(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.


% read_number(-Number)
% Unifies Number with input number from console
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).



% === GUI ===
split_list(List, Part1, Part2Length, Part2) :-
    length(Part1, N),
    length(Part2, Part2Length),
    append(Part1, Part2, List),
    length(List, ListLength),
    ListLength =:= N + Part2Length.
  
tower_top(Tower, Top) :-
  last(Tower, Top).

  % Convert lowercase atom to uppercase atom
lowercase_to_uppercase(LowercaseAtom, UppercaseAtom) :-
  atom_chars(LowercaseAtom, [LowercaseChar]),
  char_code(LowercaseChar, LowercaseCode),
  UppercaseCode is LowercaseCode - 32,
  char_code(UppercaseChar, UppercaseCode),