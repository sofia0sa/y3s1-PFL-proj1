:- consult(utils).
:- consult(board).

% !DELETE: Apenas para testar
test_place_tower :-
  board(4, Board),
  print_board(4, Board),
  place_tower(Board, 1, 1, [x], NewBoard),
  print_board(4, NewBoard).

% !DELETE: Apenas para testar
test_get_tower :-
  board(5, Board),
  print_board(5, Board),
  get_tower(Board, 1, 2, Piece),
  write(Piece),
  t(Piece, X),
  write(X).

% !DELETE: Apenas para testar
test_empty_cell :-
  board(5, Board),
  print_board(5, Board),
  empty_cell(Board, 1, 3).

% !DELETE: Apenas para testar
test_place_pawn(X,Y,P):-
  board(4, Board),
  print_board(4, Board),
  write('board printed\n'),
  place_pawn(Board, X, Y, P, NewBoard),
  write('pawn placed\n'),
  print_board(4, NewBoard).
