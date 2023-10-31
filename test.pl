% :- consult(utils).
% :- consult(board).
% :- consult(game).

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


% !DELETE: just for testing
test_valid_moves:-
  Board = [
    [[x,o], [x], empty, [pawn], [pawn]],
    [[o], empty, empty, empty, [pawn]],
    [empty, [pawn], [x,x,o,o], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [x], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  valid_moves(Board, player1, 2, 1, ValidMoves),
  write(ValidMoves).


% !DELETE: just for testing
test_game_over:-
  Board = [
    [[x,x,x], empty, empty, [pawn], empty],
    [[pawn], [pawn], [x,x], [pawn], [pawn]],
    [[pawn], [pawn], empty, [x,x,o,x,o], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  game_over(Board, Winner),
  write(Winner).


test_value :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  GameState = [Board, player2],
  value(Board, Value),
  write('HERE Value: '), write(Value), nl.
  

test_boards_and_values :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  Player = player2,
  GameState = [Board, Player],
  get_all_moves(Board, Player, Moves),
  write('HERE Length Moves: '), length(Moves, LengthMoves), write(LengthMoves), nl,
  % write('HERE Moves: '), write(Moves), nl,
  findall(Evaluated, (
    member(Move, Moves),
    write('HERE Move: '), write(Move), nl,
    translate_move(Board, Move, Board1),
    % write('HERE Board1: '), write(Board1), nl,
    value(Board1, Player, Value1),
    % write('HERE Value1: '), write(Value1), nl,
    Evaluated = [Board1,Value1]
    % write('HERE Evaluated: '), write(Evaluated), nl
  ), EvaluatedBoards),
  write('HERE EvaluatedBoards: '), write(EvaluatedBoards), nl.


test_print_moves:-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, empty],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  GameState = [Board, player2],
  get_all_moves(Board, Player, Moves),
  write('HERE Length Moves: '), length(Moves, LengthMoves), write(LengthMoves), nl,
  write('HERE Moves: '), write(Moves), nl,
  findall(Move, (
    write('HERE xalaala'), nl,
    member(Move, Moves),
  write('HERE Move: '), write(Move), nl,
  translate_move(Board, Move, Board1)),
  % write('HERE Translated Board1: '), write(Board1), nl),
  % write('HERE Move and Board: '), write(Move), write(' - '), write(Board1), nl),
   Moves).