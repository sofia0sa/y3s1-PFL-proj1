:- consult(utils).
:- consult(board).

% valid_moves(+Board, +X, +Y, +Player, -ValidMoves)
% Calculates all the valid moves for the piece at position (X, Y) for the given player.
valid_moves(Board, X, Y, ValidMoves) :-
  get_piece(Board, X, Y, [Piece]),
  findall([NewX, NewY], (
      valid_move(Board, X, Y, NewX, NewY, Piece)
  ), ValidMoves).


% !DELETE: just for testing
test_valid_moves:-
  Board = [
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]],
    [[pawn], [pawn], [pawn], [pawn], [pawn], [pawn]]
  ],
  valid_moves(Board, 1, 1, ValidMoves),
  write(ValidMoves).
  

% valid_move(+Board, +X, +Y, +NewX, +NewY, +Player, +Piece)
% Checks if the move from (X, Y) to (NewX, NewY) is valid for the given piece and player.
valid_move(Board, X, Y, NewX, NewY, Piece) :-
  inside_board(Board, NewX, NewY),
  \+ empty_cell(Board, NewX, NewY),
  length(Piece, L), % Pawn
  write(L),
  write('\n'),
  valid_piece_distance(X, Y, NewX, NewY, L).

%pawn
%move 1 cell horizontally or vertically
valid_piece_distance(X, Y, NewX, NewY, 1) :-
  (X =:= NewX; Y =:= NewY),
  (X =:= NewX + 1; X =:= NewX - 1; Y =:= NewY + 1; Y =:= NewY - 1).

% !DELETE: just for testing
test_valid_piece_distance:-
  valid_piece_distance(1, 1, 2, 1, 1),
  valid_piece_distance(1, 1, 1, 2, 1),
  valid_piece_distance(1, 1, 2, 2, 1),
  valid_piece_distance(1, 1, 0, 1, 1),
  valid_piece_distance(1, 1, 1, 0, 1),
  valid_piece_distance(1, 1, 0, 0, 1),
  \+ valid_piece_distance(1, 1, 3, 1, 1),
  \+ valid_piece_distance(1, 1, 1, 3, 1),
  \+ valid_piece_distance(1, 1, 3, 3, 1),
  \+ valid_piece_distance(1, 1, 0, 2, 1),
  \+ valid_piece_distance(1, 1, 2, 0, 1),
  \+ valid_piece_distance(1, 1, 0, 0, 1).

%rook
%move any number of cells horizontally or vertically, until it reaches the end of the board or another piece
% valid_piece_distance(X, Y, NewX, NewY, 2) :-
%   (X =:= NewX; Y =:= NewY),

  




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
