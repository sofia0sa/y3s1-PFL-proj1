
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
      % write('X: '), write(X), write(' Y: '), write(Y), nl,
      valid_moves(Board, Player, Row, Col, ListOfMoves),
      % write('HERE Moves of type 2: '), write(ListOfMoves), nl,
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
      % write('X: '), write(X), write(' Y: '), write(Y), nl,
      valid_moves(Board, Player, Row, Col, ListOfMoves, NPieces),
      % write('HERE Npieces: '), write(NPieces), nl, 
      % write('HERE LoM: '), write(ListOfMoves), nl,
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
  % Moves1 = [],
  % Moves2 = [],
  get_moves_type_1(Board, Player, Moves1),
  get_moves_type_2(Board, Player, Moves2),
  get_moves_type_3(Board, Player, Moves3),
  % Moves3 = [],
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
 
%======================= MOVES FOR COMPUTER EASY AND HARD ===========================================================================

% move_computer(+GameState, -NewGameState, +Level)
move_computer(GameState, NewGameState, 1) :-
  [Board, Player] = GameState,
  write('HERE IN move_computer EASY (implemented)') , nl,
  get_all_moves(Board, Player, Moves),
  write('HERE Moves: '), write(Moves), nl,
  random_member(Move, Moves),
  write('HERE Move: '), write(Move), nl,
  translate_move(Board, Move, NewBoard),
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

/*
%hard mode with minimax
move_computer(GameState, NewGameState, 2) :-
  write('HERE IN move_computer HARD (to implement)') , nl,
  [Board, Player] = GameState,
  get_all_moves(Board, Player, Moves),
  findall(Delta-Board1, (
    member(Move, Moves),
    translate_move(Board, Move, Board1),
    value(Board1, Player, Value1),
    % write('HERE Value1: '), write(Value1), nl,
    second_level(Board1, Player, Value2),
    % write('HERE Value2: '), write(Value2), nl,
    Delta is Value1-Value2
    % write('HERE Delta: '), write(Delta), nl
    ), EvaluatedBoards),
  % write('HERE EvaluatedBoards: '), write(EvaluatedBoards), nl,
  sort(EvaluatedBoards, SortedBoard),
  % write('HERE SortedBoard: '), write(SortedBoard), nl,
  last(SortedBoard, Delta-NewBoard),
  write('HERE Delta: '), write(Delta), nl,
  write('HERE NewBoard: '), write(NewBoard), nl,
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

second_level(Board1, Player, Value2) :-
  change_player(Player, NewPlayer),
  % write('HERE NewPlayer: '), write(NewPlayer), nl,
  get_all_moves(Board1, NewPlayer, Moves2),
  % write('HERE Moves2: '), write(Moves2), nl,
  setof(ValueAux, (
    % write('HERE Before member\n'),
    member(Move2, Moves2),
    write('HERE Move2: '), write(Move2), nl,  
    translate_move(Board1, Move2, Board2),
    write('HERE Board2: '), write(Board2), nl,
    value(Board2, NewPlayer, ValueAux),
    write('HERE ValueAux: '), write(ValueAux), nl
  ), AuxValues),
  % write('HERE AuxValues: '), write(AuxValues), nl,
  sort(AuxValues, SortedValues),
  last(SortedValues, Value2). 
  % write('HERE BoardAux: '), write(BoardAux), nl.

*/







% !DELETE
test_move_computer :-
  Board = [
    [[x,o], [x], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, [x,x], empty, empty],
    [[o,o], empty, empty, empty, empty],
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
  write('HERE NewGameState: '), write(NewGameState), nl,
  [NewBoard, NewPlayer] = NewGameState,
  length(NewBoard, Size),
  print_board(Size, NewBoard).



move_computer(GameState, NewGameState, 2) :-
  [Board, Player] = GameState,
  get_all_moves(Board, Player, Moves),
  calculate_value(Board, Player, Moves, List),
  sort(List, SortedList),
  % write('HERE SortedList: '), write(SortedList), nl,
  last(SortedList, NewBoard-Delta),
  % write('HERE Delta: '), write(Delta), nl,
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

calculate_value(Board, Player, Moves, FinalList) :- 
  calculate_value(Board, Player, Moves, [], FinalList).

calculate_value(_, _, [], Acc, Acc):- !.
  % write('HERE IN calculate_value - Base Condition'), nl,
  % write('Final List: '), write(Acc), nl.
calculate_value(Board, Player, Moves, Acc, FinalList) :-
  % write('HERE IN calculate_value'), nl,
  % write('HERE CV1 Moves: '), write(Moves), nl,
  [CurrMove|T] = Moves,
  translate_move(Board, CurrMove, Board1),
  value(Board1, Player, Value1),

  change_player(Player, NewPlayer),
  get_all_moves(Board1, NewPlayer, Moves2),
  % write('HERE CV1 Moves2: '), write(Moves2), nl,
  \+ Moves2 = [],
  calculate_value_2(Board1, NewPlayer, Moves2, List2, Value1), %!,

  sort(List2, SortedList2),
  % write('HERE SortedList2: '), write(SortedList2), nl,
  [Delta | _] = SortedList2,

  NewAcc = [Board1-Delta | Acc],
  % write('HERE CV1 NewAcc: '), write(NewAcc), nl,
  calculate_value(Board, Player, T, NewAcc, FinalList).

calculate_value_2(Board, Player, Moves, FinalList, Value1) :- 
  calculate_value_2(Board, Player, Moves, [], FinalList, Value1).

calculate_value_2(_, _, [], Acc, Acc, _):- !.
  % write('HERE IN calculate_value_2 - Base Condition'), nl,
  % write('Final List: '), write(Acc), nl.
calculate_value_2(Board, Player, Moves, Acc, FinalList, Value1) :-
  % write('HERE IN calculate_value_2'), nl,
  % write('HERE Moves: '), write(Moves), nl,
  [CurrMove|T] = Moves,
  % write('HERE CurrMove: '), write(CurrMove), nl,
  translate_move(Board, CurrMove, Board2),
  value(Board2, Player, Value2),
  Delta is Value1 - Value2,

  NewAcc = [Delta | Acc],
  % write('HERE CV2 NewAcc: '), write(NewAcc), nl,
  calculate_value_2(Board, Player, T, NewAcc, FinalList, Value1).







%============================ GET BOARD VALUE ======================================================================

%value is board value for PLayer
value(Board, Player, Value) :-
  % write('HERE IN value') , nl,
  % write('HERE Player: '), write(Player), nl,
  iterate_board(Board, XValue, OValue),
  % write('HERE XValue: '), write(XValue), nl,
  % write('HERE OValue: '), write(OValue), nl,
  (Player == player1 -> Value is XValue - OValue; Value is OValue - XValue).


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


% !DELETE
test_value:-
  Board = [
    [[x,o], [x], empty, empty, [x,o,o,x,o]],
    [empty, empty, [x,o,x], empty, [o]],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, [x,x,x], empty],
    [empty, empty, empty, empty, empty]
  ],
  value(Board, player2, Value),
  write('HERE Value: '), write(Value), nl.


%==================================================================================================


%=============================== TRANSLATE MOVES INTO BOARDS ===================================================================
translate_move(Board, Move, NewBoard) :-
  % write('HERE IN translate_move') , nl,
  [MoveFlag, Player, X, Y, NewX, NewY, NPieces] = Move,
  % write('HERE IN translate_move') , nl,
  (MoveFlag =:= 1 ->
    % write('HERE MOVE TYPE 1'), nl,
    place_pawn(Board, NewX, NewY, Player, NewBoard);
  MoveFlag =:= 2 ->
    % write('HERE MOVE TYPE 2'), nl,
    % translate_move_2(Board, Player, X, Y, NewX, NewY, NewBoard);
    move_tower(Board, X, Y, NewX, NewY, NewBoard);
  MoveFlag =:= 3 ->
    % write('HERE MOVE TYPE 3'), nl,
    % translate_move_3(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard)
    separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard)
  ).

translate_move_1(Board, Player, X, Y, NewX, NewY, NewBoard) :-
  % write('HERE IN translate_move_1') , nl,
  place_pawn(Board, Player, NewX, NewY, NewBoard).

translate_move_2(Board, Player, X, Y, NewX, NewY, NewBoard) :-
  % write('HERE IN translate_move_2') , nl,
  move_tower(Board, Player, X, Y, NewX, NewY, NewBoard).
  
translate_move_3(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard) :-
  % write('HERE IN translate_move_3') , nl,
  separate_tower(Board, Player, X, Y, NewX, NewY, NPieces, NewBoard).