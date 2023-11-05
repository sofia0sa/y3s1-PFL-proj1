:- use_module(library(lists)).
:- use_module(library(between)).


% translate(+T, -X)
% Translates a tower T into a letter X to display on the board
translate(empty, X):- X=' '.
translate(T, X):-
    length(T, L),
    length_to_letter(L, C),
    tower_top(T, Top),
    check_color(Top, C, X).


% length_to_letter(+Length, -Letter)
% converts the length of a tower into the letter the represents the corresponding piece.
length_to_letter(1, p).
length_to_letter(2, r).
length_to_letter(3, n).
length_to_letter(4, b).
length_to_letter(5, q).
length_to_letter(X, k):-
    X > 5.


% check_color(+Top, +Letter, -X)
% Checks the color of the piece on top of the tower and returns the corresponding case.
check_color(o, C, C).
check_color(x, C, X):-
    lowercase_to_uppercase(C, X).


% replace_nth1(+Index, +List, +Value, -NewList)
% Replaces the element at the specified Index in List with Value and returns the resulting NewList
replace_nth1(1, [_|T], Value, [Value|T]).
replace_nth1(N, [H|T], Value, [H|NewT]) :-
    N > 1,
    M is N - 1,
    replace_nth1(M, T, Value, NewT).


% get_tower(+Board, +X, +Y, -Tower)
% Returns the tower at the specified X and Y coordinates on the Board
get_tower(Board, X, Y, Tower) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Tower),
    Tower \= empty.


% empty_cell(+Board, +X, +Y)
% Checks if the cell at the specified X and Y coordinates on the Board is empty
empty_cell(Board, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Tower),
    Tower == empty.


% p_m(+Len, +Board)
% Prints the matrix that represents the board
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


% p_l(+Line)
% Prints a line of the board
p_l([]) :- write('|').
p_l([C|L]):-
    write('|'),
    p_c(C),
    p_l(L).


% p_c(+C)
% Prints a cell of the board, translating the tower (list of pieces) into a letter
p_c(C):-
    translate(C, S),
    format(' ~s ', [S]).


%p_hl(+N)
%print horizontal line with the length of N board cells
p_hl(0) :-
    write('|\n'), !.
p_hl(N):-
    write('|---'),
    N1 is N-1,
    p_hl(N1).


% p_h(+X, +Y)
% Prints the horizontal (X) coordinates of the board
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


% board(+Size, -Board)
% Returns a board of the specified Size
% board(4, [
%     [[o,o], empty, [x], [x]],
%     [empty, empty, empty, empty],
%     [empty, empty, empty, empty],
%     [empty, empty, empty, empty]
% ]).
board(4, [
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty]
]).
board(5, [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, [x], [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ]).


% display_game(+Size, +Board)
% Prints the board and it's  coordinates
display_game(Size, Board):-
    p_h(1, Size),
    p_m(Size, Board),
    write('\n').


% inside_board(+Board, +X, +Y)
% Checks if the coordinates X and Y are inside the Board
inside_board(Board, X, Y) :-
    length(Board, Size),
    between(1, Size, X),
    between(1, Size, Y).


%---------------------------------%

% !WARNING: Isto não está de acordo com o enuncidado
% initial_state(+Size,-Board)
% !TODO: description
initial_state(Size, Board):-
    board(Size, Board),
    display_game(Size, Board). % !WARNING: Apenas para testar
