:- consult(utils).
:- consult(board).


% separate_tower(+Board, +X, +Y, +NewX, +NewY, -NewBoard)
% !TODO: documentation
separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard) :-
  % write('HERE IN separate_tower\n'),  
  get_tower(Board, X, Y, Tower),
  % write('HERE Tower: '), write(Tower), nl,
  % write('HERE NPieces: '), write(NPieces), nl,
  split_list(Tower, Part1, NPieces, Part2),
  % write('HERE Part1: '), write(Part1), nl,
  % write('HERE Part2: '), write(Part2), nl,
  length(Board, Size),
  % print_board(Size, Board),
  place_tower(Board, X, Y, Part1, Board1),
  % print_board(Size, Board1),
  move_pieces(Board1, NewX, NewY, Part2, NewBoard).
  % print_board(Size, NewBoard).

% move_tower(+Board, +X, +Y, +NewX, +NewY, -NewBoard)
% Moves the tower at (X, Y) to (NewX, NewY) and returns the new board.
move_tower(Board, X, Y, NewX, NewY, NewBoard) :-
  get_tower(Board, X, Y, Tower),
  place_tower(Board, X, Y, empty, Board1),
  move_pieces(Board1, NewX, NewY, Tower, NewBoard).

% move_pieces(+Board, +X, +Y, +NPieces, -NewBoard)
% Moves NPieces to the top of the tower at (X,Y) and returns the new board.
move_pieces(Board, X, Y, NPieces, NewBoard):-
  get_tower(Board, X, Y, Tower),
  append(Tower, NPieces, NewTower),
  place_tower(Board, X, Y, NewTower, NewBoard).




% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, Player, X, Y, ListOfMoves) :-
  get_tower(Board, X, Y, Tower),
  \+ empty_cell(Board, X, Y),
  findall([NewX, NewY], (
      valid_move(Board, Player, X, Y, NewX, NewY, Tower)
  ), ValidMoves),
  sort(ValidMoves, ListOfMoves).


% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, Player, X, Y, ListOfMoves, NPieces) :-
  get_tower(Board, X, Y, Tower),
  \+ empty_cell(Board, X, Y),
  %check_if_can_place_tower(Board, X, Y, NPieces),
  findall([NewX, NewY], (
      valid_move(Board, Player, X, Y, NewX, NewY, Tower, NPieces)
  ), ValidMoves),
  sort(ValidMoves, ListOfMoves).
  

% Checks if the move from (X, Y) to (NewX, NewY) is valid for the given piece and player.
valid_move(Board, Player, X, Y, NewX, NewY, Tower) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Tower, L), % Pawn
  valid_piece_movement(Board, X, Y, NewX, NewY, L),
  tower_top(Tower, Top),
  check_possible_tower(Board, Player, NewX, NewY, L, Top).

valid_move(Board, Player, X, Y, NewX, NewY, Tower, NPieces) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Tower, L),
  valid_piece_movement(Board, X, Y, NewX, NewY, L),
  tower_top(Tower, Top),
  check_possible_tower(Board, Player, NewX, NewY, NPieces, Top).


check_possible_tower(Board, Player, NewX, NewY, L, Top):-
  get_tower(Board, NewX, NewY, Tower),
  length(Tower, L1),
  L2 is L1+L,
  (L2 >= 6 ->
    top_to_player(Top, Player);
    L2 < 6
  ).
  

% valid_piece_movement(+Board, +X, +Y, -NewX, -NewY, +TowerHeight)
% Returns the coordinates (NewX, NewY) of a valid move for the piece at (X, Y) with the given height.
% Pawn - can move 1 cell horizontally or vertically
valid_piece_movement(_, X, Y, NewX, NewY, 1) :-
  (X =:= NewX; Y =:= NewY),
  (X =:= NewX + 1; X =:= NewX - 1; Y =:= NewY + 1; Y =:= NewY - 1).

% Rook - can move any number of cells horizontally or vertically, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 2) :-
  X = NewX,
  vertical_up(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 2) :-
  X = NewX,
  vertical_down(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 2) :-
  Y = NewY,
  horizontal_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 2) :-
  Y = NewY,
  horizontal_right(Board, X, Y, NewX, NewY).

% Knight - can move 2 cells horizontally or vertically and then 1 cell in the other direction
valid_piece_movement(_, X, Y, NewX, NewY, 3) :-
  (abs(X - NewX) =:= 2, abs(Y - NewY) =:= 1 ; abs(X - NewX) =:= 1, abs(Y - NewY) =:= 2).

% Bishop - can move any number of cells diagonally, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  up_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  up_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  down_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  down_left(Board, X, Y, NewX, NewY).

% Queen - can move any number of cells horizontally, vertically or diagonally, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  X = NewX,
  vertical_up(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  X = NewX,
  vertical_down(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  Y = NewY,
  horizontal_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  Y = NewY,
  horizontal_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  up_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  up_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  down_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  down_left(Board, X, Y, NewX, NewY).

% !TEST: after commenting second cut
% vertical_up(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the vertical line from (X, Y) to (X, 1)
vertical_up(Board, X, Y, OccupiedX, OccupiedY) :-
  Z is Y - 1,
  between_rev(1, Z, Y1),
  \+ empty_cell(Board, X, Y1), !,
  OccupiedX = X,
  OccupiedY = Y1. %, !.

% vertical_down(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the vertical line from (X, Y) to (X, Size)
vertical_down(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  Z is Y + 1,
  between(Z, Size, Y1), 
  \+ empty_cell(Board, X, Y1), !,
  OccupiedX = X,
  OccupiedY = Y1. %, !.

% horizontal_left(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the horizontal line from (X, Y) to (1, Y)
horizontal_left(Board, X, Y, OccupiedX, OccupiedY) :-
  Z is X - 1,
  between_rev(1, Z, X1),
  \+ empty_cell(Board, X1, Y), !,
  OccupiedX = X1,
  OccupiedY = Y. %, !.
  
% horizontal_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the horizontal line from (X, Y) to (Size, Y)
horizontal_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  Z is X + 1,
  between(Z, Size, X1),
  \+ empty_cell(Board, X1, Y), !,
  OccupiedX = X1,
  OccupiedY = Y. %, !.

% up_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the top right corner
up_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  X1 is X + 1, X1 =< Size,
  Y1 is Y - 1, Y1 >= 1,
  (empty_cell(Board, X1, Y1) ->
    up_right(Board, X1, Y1, OccupiedX, OccupiedY);
    OccupiedX is X1,
    OccupiedY is Y1, !
  ).

% up_left(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the top left corner
up_left(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  X1 is X - 1, X1 >= 1,
  Y1 is Y - 1, Y1 >= 1,
  (empty_cell(Board, X1, Y1) ->
    up_left(Board, X1, Y1, OccupiedX, OccupiedY);
    OccupiedX is X1,
    OccupiedY is Y1, !
  ).

% down_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the bottom right corner
down_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  X1 is X + 1, X1 =< Size,
  Y1 is Y + 1, Y1 =< Size,
  (empty_cell(Board, X1, Y1) ->
    down_right(Board, X1, Y1, OccupiedX, OccupiedY);
    OccupiedX is X1,
    OccupiedY is Y1, !
  ).

% down_left(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the bottom left corner
down_left(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  X1 is X - 1, X1 >= 1,
  Y1 is Y + 1, Y1 =< Size,
  (empty_cell(Board, X1, Y1) ->
    down_left(Board, X1, Y1, OccupiedX, OccupiedY);
    OccupiedX is X1,
    OccupiedY is Y1, !
  ).

/*
% bishop move
% move any number of cells diagonally, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  abs(X - NewX) =:= abs(Y - NewY),
  diagonal_up_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  abs(X - NewX) =:= abs(Y - NewY),
  diagonal_up_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  abs(X - NewX) =:= abs(Y - NewY),
  diagonal_down_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  abs(X - NewX) =:= abs(Y - NewY),
  diagonal_down_left(Board, X, Y, NewX, NewY).

% diagonal_up_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the top right corner
diagonal_up_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  between(1, X, X1),
  between(1, Y, Y1),
  X2 is X1 - 1,
  Y2 is Y1 - 1,
  X2 >= 1, Y2 >= 1,
  X2 =:= X - abs(Y - Y1), Y2 =:= Y - abs(X - X1),
  \+ empty_cell(Board, X1, Y1), !,
  OccupiedX = X1,
  OccupiedY = Y1.

% diagonal_up_left(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the top left corner
diagonal_up_left(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  between(1, X, X1),
  between(Y, Size, Y1),
  X2 is X1 - 1,
  Y2 is Y1 + 1,
  X2 >= 1, Y2 =< Size,
  X2 =:= X - abs(Y - Y1), Y2 =:= Y + abs(X - X1),
  \+ empty_cell(Board, X1, Y1), !,
  OccupiedX = X1,
  OccupiedY = Y1.

% diagonal_down_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the bottom right corner
diagonal_down_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  between(X, Size, X1),
  between(1, Y, Y1),
  X2 is X1 + 1,
  Y2 is Y1 - 1,
  X2 =< Size, Y2 >= 1,
  X2 =:= X + abs(Y - Y1), Y2 =:= Y - abs(X - X1),
  \+ empty_cell(Board, X1, Y1), !,
  OccupiedX = X1,
  OccupiedY = Y1.

% diagonal_down_left(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the diagonal line from (X, Y) to the bottom left corner
diagonal_down_left(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  between(X, Size, X1),
  between(Y, Size, Y1),
  X2 is X1 + 1,
  Y2 is Y1 + 1,
  X2 =< Size, Y2 =< Size,
  X2 =:= X + abs(Y - Y1), Y2 =:= Y + abs(X - X1),
  \+ empty_cell(Board, X1, Y1), !,
  OccupiedX = X1,
  OccupiedY = Y1.
*/






/*
% bishop move
% move any number of cells diagonally, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  diagonal_down_right(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  diagonal_up_left(Board, X, Y, NewX, NewY).
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  diagonal_up_right(Board, X, Y, NewX, NewY).


diagonal_up_left(Board, X,  Y, OccupiedX, OccupiedY) :-
  Min is min(X, Y),
  Z is Min - 1, %desvio max até chegar à borda
  ul_between(Z, X, Y, OccupiedX, OccupiedY), 
  \+ empty_cell(Board, OccupiedX, OccupiedY), !.

ul_between(Z, X, Y, NewX, NewY):- 
  between(1, Z, D), !,
  NewX is X - D,
  NewY is Y - D.

diagonal_up_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  % Z1 is Size-max(X, Y),
  % Z2 is min(X, Y)-1,
  Z1 is Size - X,
  Z2 is Y - 1,
  Z is min(Z1, Z2),
  ur_between(Z, X, Y, OccupiedX, OccupiedY),
  \+ empty_cell(Board, OccupiedX, OccupiedY), !.

ur_between(Z, X, Y, NewX, NewY):- 
  between(1, Z, D),
  NewX is X + D,
  NewY is Y - D.

diagonal_down_right(Board, X, Y, OccupiedX, OccupiedY) :-
  Max is max(X, Y),
  length(Board, Size),
  Z is Size - Max, %desvio max até chegar à borda
  dr_between(Z, X, Y, OccupiedX, OccupiedY),
  \+ empty_cell(Board, OccupiedX, OccupiedY), !.

%down_right between
dr_between(Z, X, Y, NewX, NewY) :-
  between(1, Z, D), %!,
  NewX is X + D,
  NewY is Y + D.
*/
  

% diagonal_down_right(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Finds the first non-empty cell in the diagonal line down and to the right of (X, Y)
% diagonal_down_right(Board, X, Y, OccupiedX, OccupiedY) :-
%   Max is max(X, Y),
%   length(Board, Size),
%   Z is Size - Max, %desvio max até chegar à borda
%   dr_between(Z, X, Y, OccupiedX, OccupiedY), !,
%   \+ empty_cell(Board, OccupiedX, OccupiedY), !.

% dr_between(+Z, +X, +Y, -OccupiedX, -OccupiedY)
% Finds the first non-empty cell in the diagonal line down and to the right of (X, Y)
% dr_between(Z, X, Y, OccupiedX, OccupiedY) :-
%   between(1, Z, D),
%   NewX is X + D,
%   NewY is Y + D,
%   (empty_cell(Board, NewX, NewY) ->
%     dr_between(Z, NewX, NewY, OccupiedX, OccupiedY)
%   ;
%     OccupiedX = NewX,
%     OccupiedY = NewY, !
%   ).

% dr_between(+Z, +X, +Y, -OccupiedX, -OccupiedY)
% Finds the first non-empty cell in the diagonal line down and to the right of (X, Y)
% dr_between(Z, X, Y, OccupiedX, OccupiedY) :-
%   dr_between(Z, X, Y, X, Y, OccupiedX, OccupiedY).

% % dr_between(+Z, +X, +Y, +CurrentX, +CurrentY, -OccupiedX, -OccupiedY)
% % Finds the first non-empty cell in the diagonal line down and to the right of (X, Y)
% dr_between(Z, X, Y, CurrentX, CurrentY, OccupiedX, OccupiedY) :-
%   NewX is CurrentX + 1,
%   NewY is CurrentY + 1,
%   (NewX > Z ->
%     OccupiedX = CurrentX,
%     OccupiedY = CurrentY
%   ;
%     (empty_cell(Board, NewX, NewY) ->
%       dr_between(Z, X, Y, NewX, NewY, OccupiedX, OccupiedY)
%     ;
%       OccupiedX = NewX,
%       OccupiedY = NewY
%     )
%   ).


% diagonal_occupied(+X1, +Y1, +X2, +Y2)
% Checks if there are any occupied cells in the diagonal line as (X1, Y1) to (X2, Y2)
% diagonal_occupied(Board, X1, Y1, X2, Y2) :-
%   between(1, abs(X1 - X2), D),
%   X is min(X1, X2) + D,
%   Y is min(Y1, Y2) + D,
%   \+ empty_cell(Board, X, Y).



% ====================
iterate_board(Board, Top):-
  length(Board, Rows),
  between(1, Rows, Row),
  nth1(Row, Board, RowList),
  length(RowList, Cols),
  between(1, Cols, Col),
  nth1(Col, RowList, Tower),
  Tower \= empty,
  length(Tower, L),
  % L =:= 6,  %KING with only 6
  L >= 6,
  tower_top(Tower, Top).

top_to_player(x, player1).
top_to_player(o, player2).

game_over(Board, Winner):-
  iterate_board(Board, Top),
  top_to_player(Top, Winner).


% choose_piece_and_move(+Board, -NewBoard)
% Allows the user to select a piece to move and then choose a valid move for that piece.
choose_piece_and_move(Board, NewBoard) :-
  repeat,
  write('Select the piece you want to move (X, Y): '),
  get_coordinate(Board, X, Y),
  get_tower(Board, X, Y, Piece),
  valid_moves(Board, X, Y, ValidMoves),
  print_valid_moves(ValidMoves),
  write('Choose a move (NewX, NewY): '),
  get_coordinate(Board, NewX, NewY),
  member([NewX, NewY], ValidMoves), % Ensure the selected move is valid
  move_pieces(Board, X, Y, NewX, NewY, NewBoard), !.

% print_valid_moves(+ValidMoves)
% Prints the valid moves to the console for the user to choose from.
print_valid_moves([]).
print_valid_moves([[X, Y] | Rest]) :-
  format('Valid move: (~d, ~d)\n', [X, Y]),
  print_valid_moves(Rest).
