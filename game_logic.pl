:- consult(utils).
:- consult(board).

% separate_tower(+Board, +X, +Y, +Player, -NewBoard)
% Separates the tower at position (X, Y) into two towers, one with the given player and the other with the remaining pieces.
separate_tower(Board, X, Y, NMove, NPieces, NewBoard) :-
  write('HERE IN separate_tower'), nl,
  [NewX, NewY] = NMove,
  get_tower(Board, X, Y, Tower),
  split_list(Tower, Part1, NPieces, Part2),
  place_tower(Board, X, Y, Part1, Board1),
  move_pieces(Board1, NewX, NewY, Part2, NewBoard).

%move whole tower
move_tower(Board, X, Y, NMove, NewBoard) :-
  [NewX, NewY] = NMove,
  get_tower(Board, X, Y, Tower),
  place_tower(Board, X, Y, empty, Board1),
  move_pieces(Board1, NewX, NewY, Tower, NewBoard).

move_pieces(Board, X, Y, NPieces, NewBoard):-
  get_tower(Board, X, Y, Tower),
  append(Tower, NPieces, NewTower),
  place_tower(Board, X, Y, NewTower, NewBoard).



% valid_moves(+Board, +X, +Y, +Player, -ValidMoves)
% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, X, Y, ValidMoves) :-
  get_tower(Board, X, Y, Tower),
  \+ empty_cell(Board, X, Y),
  findall([NewX, NewY], (
      valid_move(Board, X, Y, NewX, NewY, Tower)
  ), ValidMoves).

% valid_moves(+Board, +X, +Y, +Player, -ValidMoves, +NPieces)
% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, X, Y, ValidMoves, NPieces) :-

  get_tower(Board, X, Y, Tower),
  \+ empty_cell(Board, X, Y),
  %check_if_can_place_tower(Board, X, Y, NPieces),
  findall([NewX, NewY], (
      valid_move(Board, X, Y, NewX, NewY, Tower, NPieces)
  ), ValidMoves).
  

% valid_move(+Board, +X, +Y, +NewX, +NewY, +Player, +Piece)
% Checks if the move from (X, Y) to (NewX, NewY) is valid for the given piece and player.
valid_move(Board, X, Y, NewX, NewY, Piece) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Piece, L), % Pawn
  % get_board_size(Board, Size),
  % L1 is L+1, %!DOUBT:
  % L1=<6,
  valid_piece_movement(Board, X, Y, NewX, NewY, L),
  check_max_tower(Board, NewX, NewY, L).
  % falta verificar altura max da torre

valid_move(Board, X, Y, NewX, NewY, Piece, NPieces) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Piece, L),
  % write('NPieces: '), write(NPieces), nl,
  % L1 is L+NPieces, %!DOUBT:
  % L1=<6,
  valid_piece_movement(Board, X, Y, NewX, NewY, L),
  check_max_tower(Board, NewX, NewY, NPieces).


%check if addition <= 6
check_max_tower(Board, NewX, NewY, L):-
  get_tower(Board, NewX, NewY, Tower),
  length(Tower, L1),
  L2 is L1+L,
  L2=<6.

% valid_piece_movement(+Board, +X, +Y, -NewX, -NewY, +Piece)
%pawn move
%move 1 cell horizontally or vertically
valid_piece_movement(_, X, Y, NewX, NewY, 1) :-
  (X =:= NewX; Y =:= NewY),
  (X =:= NewX + 1; X =:= NewX - 1; Y =:= NewY + 1; Y =:= NewY - 1).

%knight move
%move 2 cells horizontally or vertically and then 1 cell in the other direction
valid_piece_movement(_, X, Y, NewX, NewY, 3) :-
  (abs(X - NewX) =:= 2, abs(Y - NewY) =:= 1 ; abs(X - NewX) =:= 1, abs(Y - NewY) =:= 2).

% queen move
% move any number of cells horizontally, vertically or diagonally, until it reaches the end of the board or another piece
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  (X =:= NewX; Y =:= NewY ; abs(X - NewX) =:= abs(Y - NewY)),
  \+ (same_line_occupied(Board, X, Y, NewX, NewY) ; diagonal_occupied(Board, X, Y, NewX, NewY)).
  
% rook move
% move any number of cells horizontally or vertically, until it reaches another piece. Can only move to cells that are not empty.
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


between_rev(Lower, Upper, X) :- 
  Upper >= Lower, 
  X = Upper. 

between_rev(Lower, Upper, X) :- 
    Upper > Lower, 
    NewUpper is Upper - 1, 
    between_rev(Lower, NewUpper, X).

% vertical_up(+Board, +X, +Y, -OccupiedX, -OccupiedY)
% Checks if there are any occupied cells in the vertical line from (X, 1) to (X, Y)
vertical_up(Board, X, Y, OccupiedX, OccupiedY) :-
  Z is Y - 1,
  between_rev(1, Z, Y1),
  \+ empty_cell(Board, X, Y1), !,
  OccupiedX = X,
  OccupiedY = Y1, !.


vertical_down(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  Z is Y + 1,
  between(Z, Size, Y1), 
  \+ empty_cell(Board, X, Y1), !,
  OccupiedX = X,
  OccupiedY = Y1, !.


horizontal_left(Board, X, Y, OccupiedX, OccupiedY) :-
  Z is X - 1,
  between_rev(1, Z, X1),
  \+ empty_cell(Board, X1, Y), !,
  OccupiedX = X1,
  OccupiedY = Y, !.
  


horizontal_right(Board, X, Y, OccupiedX, OccupiedY) :-
  length(Board, Size),
  Z is X + 1,
  between(Z, Size, X1),
  \+ empty_cell(Board, X1, Y), !,
  OccupiedX = X1,
  OccupiedY = Y, !.

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
  % between(1, Z, D),
  % OccupiedX is X - D,
  % OccupiedY is Y - D,
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

% !DELETE: just for testing
test_valid_moves:-
  Board = [
    [[x,x], [x,o], empty, [pawn], [pawn]],
    [[pawn], [pawn], empty, empty, [pawn]],
    [[pawn], [pawn], [x,x,o,o], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [x], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  valid_moves(Board, 3, 3, ValidMoves),
  write(ValidMoves).

% diagonal_occupied(+X1, +Y1, +X2, +Y2)
% Checks if there are any occupied cells in the diagonal line as (X1, Y1) to (X2, Y2)
diagonal_occupied(Board, X1, Y1, X2, Y2) :-
  between(1, abs(X1 - X2), D),
  X is min(X1, X2) + D,
  Y is min(Y1, Y2) + D,
  \+ empty_cell(Board, X, Y).



% ====================
iterate_board(Board, Top):-
  length(Board, Rows),
  between(1, Rows, Row),
  nth1(Row, Board, RowList),
  length(RowList, Cols),
  between(1, Cols, Col),
  nth1(Col, RowList, Piece),
  Piece \= empty,
  length(Piece, L),
  L =:= 6,
  tower_top(Piece, Top).

top_to_player(x, player1).
top_to_player(o, player2).

game_over(Board, Winner):-
  iterate_board(Board, Top),
  top_to_player(Top, Winner).

% !DELETE: just for testing
test_check_winner:-
  Board = [
    [[x,x,x], empty, empty, [pawn], empty],
    [[pawn], [pawn], [x,x], [pawn], [pawn]],
    [[pawn], [pawn], empty, [x,x,o,x,o], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  check_winner(Board, Winner),
  write(Winner).





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
