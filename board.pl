:- use_module(library(lists)).
:- use_module(library(between)).
% :- consult(data).
:- consult(utils).

t(empty, X):- X=' '.

% t is a list
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


% place_tower(+Board, +X, +Y, +Piece, -NewBoard)
% Places a piece of type Piece on the Board at the specified X and Y coordinates and returns the resulting NewBoard
place_tower(Board, X, Y, Piece, NewBoard) :-
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





% get_tower(+Board, +X, +Y, -Piece)
% Returns the piece at the specified X and Y coordinates on the Board
get_tower(Board, X, Y, Piece) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    Piece \= empty.


% empty_cell(+Board, +X, +Y)
% Checks if the cell at the specified X and Y coordinates on the Board is empty
empty_cell(Board, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    Piece == empty.
    % Piece is empty.


% % in_bounds(+X, +Y)
% % Checks if the specified X and Y coordinates are within the bounds of the board
% in_bounds(Board, X, Y) :-
%     get_board_size(Board, Size),
%     between(1, Size, X),
%     between(1, Size, Y).


place_pawn(Board, X, Y, player1, NewBoard) :-
    empty_cell(Board, X, Y),
    place_tower(Board, X, Y, [x], NewBoard).
place_pawn(Board, X, Y, player2, NewBoard) :-
    empty_cell(Board, X, Y),
    place_tower(Board, X, Y, [o], NewBoard).

% can only place a new piece if the cell is empty
% place_tower(+Board, +X, +Y, +Player, -NewBoard)
% Places a pawn from white or black depending on the player on the Board at the specified X and Y coordinates and returns the resulting NewBoard
% place_pawn(Board, X, Y, Player, NewBoard) :-
%     empty_cell(Board, X, Y), 
%     %check which player is playing
%     (Player == player1 -> place_tower(Board, X, Y, [x], NewBoard);
%     Player == player2 -> place_tower(Board, X, Y, [o], NewBoard)).


move_pieces(Board, X, Y, NewX, NewY, NewBoard) :-
    % !TODO:
    place_tower(Board, NewX, NewY, Piece, NewBoard).

% !DELETE: Apenas para testar
test_move_pieces :-
    board(4, Board),
    print_board(4, Board),
    place_pawn(Board, 1, 1, player1, NewBoard),
    print_board(4, NewBoard).
    % move_pieces(Board, 1, 1, 2, 2, NewBoard),
    % print_board(4, NewBoard).

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
p_h(1, Y) :-
    write('\n'),
    format('    ~d  ', [1] ), !,
    p_h(2, Y).
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
    [[x,o], [x,o,o,x,o], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

print_board(Size, Board):-
    % clear_console,
    % write('  1   2   3   4   5'), nl,
    p_h(1, Size),
    % board(Size, B),
    p_m(Size, Board),
    write('\n').

% !TEST:
%get board size
get_board_size(Board, Size):-
    length(Board, Size).

% !TEST:
%clear board
clear_board(Board):-
    get_board_size(Board, Size),
    board(Size, Board).

% inside_board(+Board, +X, +Y)
% Checks if the coordinates X and Y are inside the Board
inside_board(Board, X, Y) :-
    get_board_size(Board, Size),
    between(1, Size, X),
    between(1, Size, Y).

    % X > 0, X =< Size,
    % Y > 0, Y =< Size.

% !DELETE: Apenas para testar
test_inside_board :-
    board(4, Board),
    print_board(4, Board),
    inside_board(Board, 1, 1),
    inside_board(Board, 4, 4),
    inside_board(Board, 2, 3),
    \+ inside_board(Board, 0, 0),
    \+ inside_board(Board, 5, 5),
    \+ inside_board(Board, 1, 5),
    \+ inside_board(Board, 5, 1).




%---------------------------------%

% init_state(+Size,-Board)
% Unifies Board with a Size matrix that represents the game: animals and empty pieces
init_state(Size, Board):-
    board(Size, Board),
    print_board(Size, Board). % !WARNING: Apenas para testar
    %fill_water(Size).