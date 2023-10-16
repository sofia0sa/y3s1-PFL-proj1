:- use_module(library(lists)).
% :- consult(data).
% :- consult(utils).

t(empty, X):- X=' '.

%print matrix
p_m([]) :- p_hl(5).
p_m([L|T]):-
    p_hl(5), nl,
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
    write(' '),
    t(C, S),
    write(S),
    write(' ').

%print horizontal line
p_hl(0) :-
    write('|\n'), !.
p_hl(N):-
    write('|---'),
    N1 is N-1,
    p_hl(N1).



% 5x5 empty board
board([
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

print_board :-
    write('  1   2   3   4   5'), nl,
    board(B),
    p_m(B).