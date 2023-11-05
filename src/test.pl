:- consult(utils).
:- consult(board).
:- consult(game).

% !DELETE
test_value:-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o,x]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  % value(Board, player2, Value),
  value(Board, Value),
  write('HERE Value: '), write(Value), nl.

% !DELETE
test_get_all_moves:-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  get_all_moves(Board, player2, Moves),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).

% !DELETE
test_get_moves_by_type :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
],
  get_moves_by_type(Board, player2, Moves, 2),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).


% !DELETE
test_get_moves_by_type :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  % valid_moves(Board, player1, 2, 1, ListOfMovesMoves),
  % write('HERE list: '), write(ListOfMovesMoves), nl,
  get_moves_by_type(Board, player2, Moves, 3),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).

% !DELETE
test_get_moves_by_type :-
  Board = [
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [[x,x,x], empty, [x], [x], [x,x]],
    [[x,x,x], [x], [x], [x], [x,x]]
  ],
  get_moves_by_type(Board, player1, Moves, 1),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).


% !DELETE: apenas para testes
test_split_list :-
  split_list([1,2,3,4,5,6,7,8,9], Part1, 3, Part2),
  write('Part1: '), write(Part1), nl,
  write('Part2: '), write(Part2), nl.

% !DELETE: Apenas para testar
test_place_tower :-
  board(4, Board),
  display_game(4, Board),
  place_tower(Board, 1, 1, [x], NewBoard),
  display_game(4, NewBoard).

% !DELETE: Apenas para testar
test_get_tower :-
  board(5, Board),
  display_game(5, Board),
  get_tower(Board, 1, 2, Piece),
  write(Piece),
  translate(Piece, X),
  write(X).

% !DELETE: Apenas para testar
test_empty_cell :-
  board(5, Board),
  display_game(5, Board),
  empty_cell(Board, 1, 3).

% !DELETE: Apenas para testar
% test_place_pawn(X,Y,P):-
%   board(4, Board),
%   display_game(4, Board),
%   write('board printed\n'),
%   place_pawn(Board, X, Y, P, NewBoard),
%   write('pawn placed\n'),
%   display_game(4, NewBoard).


% !DELETE: just for testing
test_valid_moves:-
  Board = [
    [[x,o], [x], empty, [x], [x,o]],
    [[o], empty, [x,x,o,o,x], empty, [o]],
    [empty, [o], empty, empty, empty],
    [[x,o], [x], [x,x,x,o,x], empty, [x]],
    [[o,x], empty, [o], empty, [x,o,x]]
  ],
  display_game(5, Board),  
  valid_moves(Board, player1, 3, 4, ValidMoves),
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


  % !DELETE: Apenas para testar
test_inside_board :-
  board(4, Board),
  display_game(4, Board),
  inside_board(Board, 1, 1),
  inside_board(Board, 4, 4),
  inside_board(Board, 2, 3),
  \+ inside_board(Board, 0, 0),
  \+ inside_board(Board, 5, 5),
  \+ inside_board(Board, 1, 5),
  \+ inside_board(Board, 5, 1).


% !DELETE
test_move_computer :-
  Board = [
    [[x,o], [x], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, [x,x,x], empty, empty],
    [empty, empty, empty, empty, [x,o,o,x,o]],
    [empty, empty, empty, empty, empty]
  ],
  % Board = [
  %   [[x,o], [x], empty, empty, [x,o,o,x,o]],
  %   [empty, empty, [x,o,x], empty, [o]],
  %   [empty, empty, empty, empty, empty],
  %   [[o,o], empty, empty, [x,x,x], empty],
  %   [empty, empty, empty, empty, empty]
  % ],
  GameState = [Board, player1],
  move_computer(GameState, NewGameState, 2),
  % write('HERE NewGameState: '), write(NewGameState), nl,
  [NewBoard, _NewPlayer] = NewGameState,
  length(NewBoard, Size),
  display_game(Size, NewBoard).


% !DELETE
test_place_pawn :-
  Board = [
    [empty, empty, empty, empty],
    [[x], [x], [x], [x]],
    [[x], [x], [x], [x]],
    [[x], [x], [x], [x]]
  ],
  place_pawn(Board, 1, 1, player1, NewBoard), %se isto falha, nao passa para baixo
  length(NewBoard, Size),
  display_game(Size, NewBoard).

% !DELETE
test_count_pieces :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, [x], [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  count_pieces(Board, o, Count),
  format('Count: ~w\n', [Count]).

% !DELETE
test_flatten :-
  flatten([1, [2, 3], [4, [5, 6]]], FlatList),
  write(FlatList).
