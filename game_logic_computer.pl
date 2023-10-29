

% A move is of this type [MoveFlag, Player, X, Y, NewX, NewY, NPieces]

% predicate to get a list of all the moves of the type 1 -> place pawn (aka all the cells that are empty)
get_moves_type_1(Board, Player, Moves) :-
    findall([1, Player, 0, 0, NewX, NewY, -1], empty_cell(Board, NewX, NewY), Moves).


% !DELETE
test_get_moves_type_1 :-
  Board = [
    [[x,o], [x,o,o,x,o], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
],
  get_moves_type_1(Board, player1, Moves),
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
  write(Moves).


%==================================================================================================

% move_computer(+GameState, -NewGameState, +Level)
move_computer(GameState, NewGameState, 1) :-
    write('HERE IN move_computer EASY (to implement)') , nl,
    fail.

move_computer(GameState, NewGameState, 2) :-
    write('HERE IN move_computer HARD (to implement)') , nl,
    fail.