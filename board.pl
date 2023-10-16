:- use_module(library(lists)).
% :- consult(data).
:- consult(utils).

t(empty, X):- X='   '.

t(T, X):-
    length(T, L),
    write(L), nl,
    length_to_letter(L, C),
    write(C), nl,
    tower_top(T, Top),
    write(Top), nl,
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

% !WARNING: Apenas para testar
print_piece :-
    T = [o, x],
    t(T, X),
    write(X).



%print matrix
p_m([]) :- p_hl(5).
p_m([L|T]):-
    p_hl(5),
    p_l(L), nl,
    p_m(T).

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
    write(S).

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
    format('  ~d ', [1] ),
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
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

print_board(Size, Board):-
    % write('  1   2   3   4   5'), nl,
    p_h(1, Size),
    % board(Size, B),
    p_m(Board).


%---------------------------------%

% init_state(+Size,-Board)
% Unifies Board with a Size matrix that represents the game: animals and empty pieces
init_state(Size, Board):-
    board(Size, Board),
    print_board(Size, Board). % !WARNING: Apenas para testar
    %fill_water(Size).