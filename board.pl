:- use_module(library(lists)).
% :- consult(data).
% :- consult(utils).



%print matrix
p_board([]).
p_m([L|T]):-
    p_l(L), nl,
    p_m(T)

%print list
p_l([]).
p_l([C|L]):-
    write('|'),
    p_c(C),
    write('|'),
    p_l(L)

%print cell
p_c().
p_c(C):-
    t(C, S)
    write(S)

%print horizontal line


print_board :-
    board(B),
    p_m(B).

