

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
      valid_moves(Board, Row, Col, ListOfMoves),
      write('HERE: '), write(ListOfMoves), nl
      ), Moves).


% !DELETE
test_get_moves_type_2 :-
  Board = [
    [[x,o], [x], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [[o,o], empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
],
  get_moves_type_2(Board, player1, Moves),
  write(Moves).
