

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
    
