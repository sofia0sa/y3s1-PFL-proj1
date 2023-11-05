% ================== DYNAMIC FACTS ==================

:- dynamic difficulty/2.
:- dynamic name_of/2.


% =============== GAME ===================

% change_player(+CurrtPlayer,-NextPlayer)
% Change the player's turn.
change_player(player1, player2).
change_player(player2, player1).

% player_case(+Player, -Case)
% Returns the case of the player's pieces.
player_case(player1, 'Uppercase').
player_case(player2, 'lowercase').

% player_char(+Player, -Char)
% Returns the character of the player's pieces.
player_char(player1, x).
player_char(player2, o).

% tower_top(+Tower, -Top)
% Returns the top element of Tower.
tower_top(Tower, Top) :-
    last(Tower, Top).
  
% top_to_player(+Top, -Player)
% Returns the player that owns the top piece.
top_to_player(x, player1).
top_to_player(o, player2).

% board_pieces(+Size, -NPieces)
% Returns the number of pieces allowed for the given board size.
board_pieces(5,16).
board_pieces(4,12).

% ================== READ INPUT ==================

% choose_number(+Min, +Max, +Context, -Value)
% Checks if the value given by user input is between Min and Max or if it's exactly the only number that the user can type.
% When there is no number interval, checks if it's the same number. 
choose_number(SameN, SameN, Context, Value):-
    repeat,
    format('~a (can only be ~d): ', [Context, SameN]),
    read_number(Value),
    Value == SameN, !.
% Checks if the number is between Min and Max.
choose_number(Min, Max, Context, Value):-
    repeat,
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    read_number(Value),
    between(Min, Max, Value), !.

% read_number(-Number)
% Unifies the Number with input number from console.
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X, Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X, X).

% get_name(+Player)
% Asks and saves a player's name. Dynamically adds the name_of/2 fact to the base fact.
get_name(Player) :-
    format('Hello ~a, what is your name? ', [Player]),
    read_line(Codes),
    atom_codes(Name, Codes),
    asserta(name_of(Player, Name)).

% ============== LIST UTILITIES ================

% split_list(+List, -Part1, +Part2Length, -Part2)
% Splits a list into two parts, Part1 and Part2, where Part2 has length Part2Length
split_list(List, Part1, Part2Length, Part2) :-
    length(Part2, Part2Length),
    append(Part1, Part2, List), !.

% flatten(+List, -FlatList)
% Flattens a nested list into a single list.
flatten([], []).
flatten([H|T], FlatList) :-
  flatten(H, FlatH),
  flatten(T, FlatT),
  append(FlatH, FlatT, FlatList).
flatten(X, [X]).

% ================== PRINTING ==================

% print_list(+List)
% Prints the elements of List to the console in the format "1 - Element1"
print_list(List) :-
    print_list(List, 1).

% print_list(+List, +Index)
% Prints the elements of List to the console in the format "Index - Element"
print_list([], _).
print_list([H|T], Index) :-
    write(Index), write(' - '), write(H), nl,
    NewIndex is Index + 1,
    print_list(T, NewIndex).

% print_tower_structure(+Tower, +Height)
% Prints the tower structure, indicating the index.
print_tower_structure(Tower, Height) :-
    write('\nTower Structure:\n'),
    print_tower_index(Height, 1),
    print_tower_structure(Tower, Height, 1).

% print_tower_structure(+Tower, +Height, +Index)
% Prints the tower structure.
print_tower_structure(Tower, Height, Height) :-
    nth1(Height, Tower, Element),
    write(Element), nl.
print_tower_structure(Tower, Height, Index) :-
    nth1(Index, Tower, Element),
    write(Element),
    NextIndex is Index + 1,
    write(' - '),
    print_tower_structure(Tower, Height, NextIndex).

% print_tower_index(+Height, +Index)
% Prints the index of the tower pieces.
print_tower_index(Height, Height) :-
    format('~d (Top)~n', [Height]).
print_tower_index(Height, Index) :-
    format('~d   ', [Index]),
    NextIndex is Index + 1,
    print_tower_index(Height, NextIndex). 


% ================== CLEARING ==================

% clear_data/0
% Clears all names and difficulties, removing them from the fact base.
clear_data :-
    retractall(difficulty(_, _)),
    retractall(name_of(_, _)).

% clear_console/0
% Clears the console.
clear_console :-
    write('\33\[2J').

% ================== OTHERS ==================  

% lowercase_to_uppercase(+LowercaseAtom, -UppercaseAtom)
% Converts lowercase atom to uppercase atom. Used to differentiate players in board.
lowercase_to_uppercase(LowercaseAtom, UppercaseAtom) :-
    atom_chars(LowercaseAtom, [LowercaseChar]),
    char_code(LowercaseChar, LowercaseCode),
    UppercaseCode is LowercaseCode - 32,
    char_code(UppercaseChar, UppercaseCode),
    atom_chars(UppercaseAtom, [UppercaseChar]).
  
  
%between_rev(+Lower, +Upper, -X)
% X is a number between Lower and Upper, in descending order. Used to determine moves in certain directions.
between_rev(Lower, Upper, Upper) :- 
    Upper >= Lower.
  between_rev(Lower, Upper, X) :- 
      Upper > Lower, 
      NewUpper is Upper - 1, 
      between_rev(Lower, NewUpper, X).


% init_random_state/0
% Responsible for initializing the random module.
init_random_state :-
    now(X),
    setrand(X).