:- consult(utils).
:- consult(board).

% valid_moves(+Board, +X, +Y, +Player, -ValidMoves)
% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, X, Y, ValidMoves) :-
  get_piece(Board, X, Y, Piece),
  findall([NewX, NewY], (
      valid_move(Board, X, Y, NewX, NewY, Piece)
  ), ValidMoves).


% !DELETE: just for testing
test_valid_moves:-
  Board = [
    [[x,x,x], empty, empty, [pawn], empty, [pawn]],
    [empty, [pawn], [x,x], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  valid_moves(Board, 3, 2, ValidMoves),
  write(ValidMoves).
  

% valid_move(+Board, +X, +Y, +NewX, +NewY, +Player, +Piece)
% Checks if the move from (X, Y) to (NewX, NewY) is valid for the given piece and player.
valid_move(Board, X, Y, NewX, NewY, Piece) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Piece, L),
  get_board_size(Board, Size),
  valid_piece_movement(Board, X, Y, NewX, NewY, L).
  % falta verificar altura max da torre


% valid_piece_movement(+Board, +X, +Y, -NewX, -NewY, +Piece)
%pawn move
%move 1 cell horizontally or vertically
valid_piece_movement(_, X, Y, NewX, NewY, 1) :-
  (X =:= NewX; Y =:= NewY),
  (X =:= NewX + 1; X =:= NewX - 1; Y =:= NewY + 1; Y =:= NewY - 1).

%knight move
%move 2 cells horizontally or vertically and then 1 cell in the other direction
valid_piece_movement(_, X, Y, NewX, NewY, 3) :-
  % (X =:= NewX + 2; X =:= NewX - 2; Y =:= NewY + 2; Y =:= NewY - 2),
  % (X =:= NewX + 1; X =:= NewX - 1; Y =:= NewY + 1; Y =:= NewY - 1).
  (abs(X - NewX) =:= 2, abs(Y - NewY) =:= 1 ; abs(X - NewX) =:= 1, abs(Y - NewY) =:= 2).

% rook move
% move any number of cells horizontally or vertically, until it reaches another piece. Can only move to cells that are not empty.
% caso 1: Manter X, mudar Y, andar na vertical até encontrar uma peça
valid_piece_movement(Board, X, Y, NewX, NewY, 2) :-
  (X =:= NewX; Y =:= NewY),
  same_line_occupied(Board, X, Y, NewX, NewY).





%dar o tabuleiro
%veirifcar limite,
%verificar o limite



% !DELETE: just for testing
test_valid_rook_move :-
  Board = [
    [[x,x,x], empty, empty, [pawn], empty, [pawn]],
    [empty, empty, [x,x], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], empty, [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], empty, [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  valid_piece_movement(Board, 3, 2, 3, 4, 2).


% bishop move
% move any number of cells diagonally, until it reaches another piece. Can only move to cells that are not empty.
valid_piece_movement(Board, X, Y, NewX, NewY, 4) :-
  abs(X - NewX) =:= abs(Y - NewY),
  \+ diagonal_occupied(Board, X, Y, NewX, NewY).


% queen move
% move any number of cells horizontally, vertically or diagonally, until it reaches the end of the board or another piece
valid_piece_movement(Board, X, Y, NewX, NewY, 5) :-
  (X =:= NewX; Y =:= NewY ; abs(X - NewX) =:= abs(Y - NewY)),
  \+ (same_line_occupied(Board, X, Y, NewX, NewY) ; diagonal_occupied(Board, X, Y, NewX, NewY)).
  

% same_line_occupied(+X1, +Y1, +X2, +Y2)
% Checks if there are any occupied cells in the same line as (X1, Y1) to (X2, Y2)
same_line_occupied(Board, X1, Y1, X2, Y2) :-
  (X1 =:= X2 ->
      X is X1,
      MinY is min(Y1, Y2),
      MaxY is max(Y1, Y2),
      between(MinY, MaxY, Y),
      \+ empty_cell(Board, X, Y);
  Y is Y1,
  MinX is min(X1, X2),
  MaxX is max(X1, X2),
  between(MinX, MaxX, X),
  \+ empty_cell(Board, X, Y)).

% diagonal_occupied(+X1, +Y1, +X2, +Y2)
% Checks if there are any occupied cells in the diagonal line as (X1, Y1) to (X2, Y2)
diagonal_occupied(Board, X1, Y1, X2, Y2) :-
  between(1, abs(X1 - X2), D),
  X is min(X1, X2) + D,
  Y is min(Y1, Y2) + D,
  \+ empty_cell(Board, X, Y).



% choose_piece_and_move(+Board, -NewBoard)
% Allows the user to select a piece to move and then choose a valid move for that piece.
choose_piece_and_move(Board, NewBoard) :-
  repeat,
  write('Select the piece you want to move (X, Y): '),
  get_coordinate(Board, X, Y),
  get_piece(Board, X, Y, Piece),
  valid_moves(Board, X, Y, ValidMoves),
  print_valid_moves(ValidMoves),
  write('Choose a move (NewX, NewY): '),
  get_coordinate(Board, NewX, NewY),
  member([NewX, NewY], ValidMoves), % Ensure the selected move is valid
  move_piece(Board, X, Y, NewX, NewY, NewBoard), !.

% print_valid_moves(+ValidMoves)
% Prints the valid moves to the console for the user to choose from.
print_valid_moves([]).
print_valid_moves([[X, Y] | Rest]) :-
  format('Valid move: (~d, ~d)\n', [X, Y]),
  print_valid_moves(Rest).
