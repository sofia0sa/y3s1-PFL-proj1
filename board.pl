:- use_module(library(lists)).
% :- consult(data).
:- consult(utils).

t(empty, X):- X=' '.

t(T, X):-
    length(T, L),
    % write(L), nl,
    length_to_letter(L, C),
    % write(C), nl,
    tower_top(T, Top),
    % write(Top), nl,
    check_color(Top, C, X).


length_to_letter(1, p).
length_to_letter(2, r).
length_to_letter(3, n).
length_to_letter(4, b).
length_to_letter(5, q).
length_to_letter(6, k).

check_color(o, C, X):-
    X = C.
check_color(x, C, X):-
    lowercase_to_uppercase(C, X).

% o -> pretas, lower case
% x -> brancas, upper case

% !DELETE: Apenas para testar
print_piece :-
    T = [o, x],
    t(T, X),
    write(X).


% place_piece(+Board, +X, +Y, +Piece, -NewBoard)
% Places a piece of type Piece on the Board at the specified X and Y coordinates and returns the resulting NewBoard
place_piece(Board, X, Y, Piece, NewBoard) :-
    nth1(Y, Board, Row),
    replace_nth1(X, Row, Piece, NewRow),
    replace_nth1(Y, Board, NewRow, NewBoard).

% replace_nth1(+Index, +List, +Value, -NewList)
% Replaces the element at the specified Index in List with Value and returns the resulting NewList
replace_nth1(1, [_|T], Value, [Value|T]).
replace_nth1(N, [H|T], Value, [H|NewT]) :-
    N > 1,
    M is N - 1,
    replace_nth1(M, T, Value, NewT).


% !DELETE: Apenas para testar
test_place_piece :-
    board(4, Board),
    print_board(4, Board),
    place_piece(Board, 3, 3, [x], NewBoard),
    print_board(4, NewBoard).


% get_piece(+Board, +X, +Y, -Piece)
% Returns the piece at the specified X and Y coordinates on the Board
get_piece(Board, X, Y, Piece) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    Piece \= empty.

% !DELETE: Apenas para testar
test_get_piece :-
    board(5, Board),
    print_board(5, Board),
    get_piece(Board, 1, 3, Piece),
    t(Piece, X),
    write(X).

% check_empty_cell(+Board, +X, +Y)
% Checks if the cell at the specified X and Y coordinates on the Board is empty
check_empty_cell(Board, X, Y) :-
    get_piece(Board, X, Y, Piece),
    Piece = empty.

% !DELETE: Apenas para testar
test_check_empty_cell :-
    board(5, Board),
    print_board(5, Board),
    check_empty_cell(Board, 1, 3).


% can onçy place a new piece if the cell is empty
% place_piece(+Board, +X, +Y, +Player, -NewBoard)
% Places a pawn from white or black depending on the player on the Board at the specified X and Y coordinates and returns the resulting NewBoard
place_pawn(Board, X, Y, Player, NewBoard) :-
    check_empty_cell(Board, X, Y),
    %check which player is playing
    (Player = player1 -> place_piece(Board, X, Y, [x], NewBoard);
    Player = player2 -> place_piece(Board, X, Y, [o], NewBoard)).

% !DELETE: Apenas para testar
test_place_pawn(X,Y,P):-
    board(4, Board),
    print_board(4, Board),
    place_pawn(Board, X, Y, P, NewBoard),
    print_board(4, NewBoard).

%---------------------------------%
%print matrix
p_m(Len, []) :- 
    write('  '),
    p_hl(Len).
p_m(Len, [L|T]):-
    write('  '),
    p_hl(Len),
    length(T, Len2),
    N is Len - Len2,
    format('~w ', [N]),
    p_l(L), nl,
    p_m(Len, T).

%print line
p_l([]) :- write('|').
p_l([C|L]):-
    write('|'),
    p_c(C),
    p_l(L).

%print cell
% p_c().
p_c(C):-
    t(C, S),
    format(' ~s ', [S]).
    % write(S).

%print horizontal line
p_hl(0) :-
    write('|\n'), !.
p_hl(N):-
    write('|---'),
    N1 is N-1,
    p_hl(N1).



%print header
p_h(X, Y) :-
    X =:= 1,
    write('  '),
    format('  ~d  ', [1] ),
    X1 is X+1,
    p_h(X1, Y).

p_h(X, X) :-
    format(' ~d \n', [X] ), !.
p_h(X, Y) :-
    format(' ~d  ', [X] ),
    X1 is X+1,
    p_h(X1, Y).


board(4, [
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty]
]).

% 5x5 empty board
board(5, [
    [[o,o], empty, empty, empty, empty],
    [[o,x,x], empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

print_board(Size, Board):-
    % write('  1   2   3   4   5'), nl,
    p_h(1, Size),
    % board(Size, B),
    p_m(Size, Board).

% !TEST:
%get board size
get_board_size(Board, Size):-
    length(Board, Size).

% !TEST:
%clear board
clear_board(Board):-
    get_board_size(Board, Size),
    board(Size, Board).





%---------------------------------%

% init_state(+Size,-Board)
% Unifies Board with a Size matrix that represents the game: animals and empty pieces
init_state(Size, Board):-
    board(Size, Board),
    print_board(Size, Board). % !WARNING: Apenas para testar
    %fill_water(Size).