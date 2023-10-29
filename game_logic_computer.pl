

% A move is of this type [MoveFlag, Player, X, Y, NewX, NewY, NPieces]

% predicate to get a list of all the moves of the type 1 -> place pawn (aka all the cells that are empty)
get_moves_type_1(Board, Player, Moves) :-
    findall([1, Player, 0, 0, NewX, NewY, -1], empty_cell(Board, NewX, NewY), Moves).


% !DELETE
test_get_moves_type_1 :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
],
  get_moves_type_1(Board, player1, Moves),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).
    
%predicate to get a list of all the moves of the type 2 -> move tower
%iterate through all the cells of the board, and find the valid moves for all the towers 
get_moves_type_2(Board, Player, Moves) :-
    length(Board, Size),
    findall([2, Player, X, Y, NewX, NewY, -1], (
      %iterate through all the cells of the board
      between(1, Size, Row),
      nth1(Row, Board, RowList),
      between(1, Size, Col),
      nth1(Col, RowList, Cell),
      %if the cell is a tower, find all the valid moves for that tower
      \+ empty_cell(Board, Row, Col),
      X is Row, Y is Col,
      write('X: '), write(X), write(' Y: '), write(Y), nl,
      valid_moves(Board, Player, Row, Col, ListOfMoves),
      write('HERE: '), write(ListOfMoves), nl,
      member([NewX, NewY], ListOfMoves)
      
      ), Moves).

translate([x,y,1], [x,y,'Adicionar uma peça em:']).

% !DELETE
test_get_moves_type_2 :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
],
  % valid_moves(Board, player1, 2, 1, ListOfMovesMoves),
  % write('HERE list: '), write(ListOfMovesMoves), nl,
  get_moves_type_2(Board, player2, Moves),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).

%predicate to get a list of all the moves of the type 3 -> separete tower and move pieces
%iterate through all the cells of the board, and find the valid moves for all the towers
get_moves_type_3(Board, Player, Moves) :-
    length(Board, Size),
    findall([3, Player, X, Y, NewX, NewY, NPieces], (
      %iterate through all the cells of the board
      between(1, Size, Row),
      nth1(Row, Board, RowList),
      between(1, Size, Col),
      nth1(Col, RowList, Cell),
      %if the cell is a tower, get the
      check_if_tower_exists(Board, Row, Col, L),
      L1 is L-1,
      between(1, L1, NPieces),
      X is Row, Y is Col,
      write('X: '), write(X), write(' Y: '), write(Y), nl,
      valid_moves(Board, Player, Row, Col, ListOfMoves, NPieces),
      write('HERE Npieces: '), write(NPieces), nl, 
      write('HERE LoM: '), write(ListOfMoves), nl,
      member([NewX, NewY], ListOfMoves)
      
      ), Moves).


% !DELETE
test_get_moves_type_3 :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  % valid_moves(Board, player1, 2, 1, ListOfMovesMoves),
  % write('HERE list: '), write(ListOfMovesMoves), nl,
  get_moves_type_3(Board, player2, Moves),
  length(Moves, L),
  write('HERE Length: '), write(L), nl,
  write(Moves).

get_all_moves(Board, Player, Moves) :-
  get_moves_type_1(Board, Player, Moves1),
  get_moves_type_2(Board, Player, Moves2),
  get_moves_type_3(Board, Player, Moves3),
  append(Moves1, Moves2, Moves12),
  append(Moves12, Moves3, Moves).


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
 
%==================================================================================================

% move_computer(+GameState, -NewGameState, +Level)
move_computer(GameState, NewGameState, 1) :-
  [Board, Player] = GameState,
  write('HERE IN move_computer EASY (to implement)') , nl,
  get_all_moves(Board, Player, Moves),
  write('HERE Moves: '), write(Moves), nl,
  random_member(Move, Moves),
  write('HERE Move: '), write(Move), nl,
  translate_move(Board, Move, NewBoard),
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

move_computer(GameState, NewGameState, 2) :-
  write('HERE IN move_computer HARD (to implement)') , nl,
  [Board, Player] = GameState,
  change_player(Player, NewPlayer),
  NewGameState = [Board, NewPlayer],
  fail.


%==================================================================================================

%value is board value for PLayer
value(GameState, Value) :-
  [Board, Player] = GameState,
  iterate_board(Board, XValue, OValue),
  (Player == player1 -> Value is XValue - OValue; Value is OValue - XValue).



test_value :-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  GameState = [Board, player2],
  value(GameState, Value),
  write('HERE Value: '), write(Value), nl.
  

/* Calculates O and X Value based on tower heights and tops*/
iterate_board(Board, FinalXValue, FinalOValue) :-
  RowIndex is 1,
  XValue is 0,
  OValue is 0,
  iterate_rows(Board, RowIndex, XValue, OValue, FinalXValue, FinalOValue).

iterate_rows([], _, XValue, OValue, XValue, OValue).
iterate_rows([Row|Rest], RowIndex, XValue, OValue, FinalXValue, FinalOValue) :-
  iterate_columns(Row, RowIndex, 1, XValue, OValue, NewXValue, NewOValue),
  NextRow is RowIndex + 1,
  iterate_rows(Rest, NextRow, NewXValue, NewOValue, FinalXValue, FinalOValue).

iterate_columns([], _, _, XValue, OValue, XValue, OValue).
iterate_columns([Cell|Rest], RowIndex, ColIndex, XValue, OValue, FinalXValue, FinalOValue) :-
  process_cell(Cell, RowIndex, ColIndex, XValue, OValue, NewXValue, NewOValue),
  NextCol is ColIndex + 1,
  iterate_columns(Rest, RowIndex, NextCol, NewXValue, NewOValue, FinalXValue, FinalOValue).

process_cell(empty, _, _, XValue, OValue, XValue, OValue).
process_cell(Cell, _, _, XValue, OValue, NewXValue, NewOValue) :-
  % Cell \= empty,
  length(Cell, TowerHeight),
  ( tower_top(Cell, Top), Top == x -> NewXValue is XValue + TowerHeight, NewOValue = OValue ;
                                    NewXValue = XValue, NewOValue is OValue + TowerHeight ).

%==================================================================================================


 
%soma das alturas das minhas torres - soma das alturas das torres do adversario

% funcao:-
% value(BoardAposMinhaJogada, Valor1)
% value(BoardAposJogadorAdversario, Valor1)
% Delta is Valor1-Valor2
% [BoardAposMinhaJogada, BoardAposJogadorAdversario, Delta] 

% ordenar lista de [B,B,Delta] por ordem decrescente de Delta e primeiro elemento (maior delta)


%==================================================================================================
translate_move(Board, Move, NewBoard) :-
  [MoveFlag, Player, X, Y, NewX, NewY, NPieces] = Move,
  write('HERE IN translate_move') , nl,
  (MoveFlag =:= 1 ->
    write('HERE MOVE TYPE 1'), nl,
    place_pawn(Board, NewX, NewY, Player, NewBoard);
  MoveFlag =:= 2 ->
    write('HERE MOVE TYPE 2'), nl,
    % translate_move_2(Board, Player, X, Y, NewX, NewY, NewBoard);
    move_tower(Board, X, Y, NewX, NewY, NewBoard);
  MoveFlag =:= 3 ->
    write('HERE MOVE TYPE 3'), nl,
    % translate_move_3(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard)
    separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard)
  ).

translate_move_1(Board, Player, X, Y, NewX, NewY, NewBoard) :-
  write('HERE IN translate_move_1') , nl,
  place_pawn(Board, Player, NewX, NewY, NewBoard).

translate_move_2(Board, Player, X, Y, NewX, NewY, NewBoard) :-
  write('HERE IN translate_move_2') , nl,
  move_tower(Board, Player, X, Y, NewX, NewY, NewBoard).
  
translate_move_3(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard) :-
  write('HERE IN translate_move_3') , nl,
  separate_tower(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard).