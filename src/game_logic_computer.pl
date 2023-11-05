
% A move is of this type [MoveFlag, Player, X, Y, NewX, NewY, NPieces]

% get_moves_by_type(+Board, +Player, -Moves, +MoveType)
% Gets all the moves of type 1 (place pawn) for a given player and board.
get_moves_by_type(Board, Player, Moves, 1) :-
    length(Board, Size),
    player_char(Player, Char),
    findall([1, Player, 0, 0, NewX, NewY, -1], ( 
      empty_cell(Board, NewX, NewY),
under_piece_limit(Board, Size, Char)
    ), Moves).

% Gets all the moves of type 2 (move tower) for a given player and board.
get_moves_by_type(Board, Player, Moves, 2) :-
  length(Board, Size),
  findall([2, Player, X, Y, NewX, NewY, -1], (
    between(1, Size, Row),
    nth1(Row, Board, RowList),
    between(1, Size, Col),
    nth1(Col, RowList, _Cell),
    \+ empty_cell(Board, Row, Col),
    X is Row, Y is Col,
    valid_moves(Board, Player, Row, Col, ListOfMoves),
    member([NewX, NewY], ListOfMoves)
    
    ), Moves).

% Gets all the moves of type 3 (separate tower) for a given player and board.
get_moves_by_type(Board, Player, Moves, 3) :-
  length(Board, Size),
  findall([3, Player, X, Y, NewX, NewY, NPieces], (
    between(1, Size, Row),
    nth1(Row, Board, RowList),
    between(1, Size, Col),
    nth1(Col, RowList, _Cell),
    check_if_tower_exists(Board, Row, Col, L),
    L1 is L-1,
    between(1, L1, NPieces),
    X is Row, Y is Col,
    valid_moves(Board, Player, Row, Col, ListOfMoves, NPieces),
    member([NewX, NewY], ListOfMoves)
    ), Moves).


% get_all_moves(+Board, +Player, -Moves)
% Gets all the moves from each type for a given player and board.
get_all_moves(Board, Player, Moves) :-
  get_moves_by_type(Board, Player, Moves1, 1),
  get_moves_by_type(Board, Player, Moves2, 2),
  get_moves_by_type(Board, Player, Moves3, 3),
  append(Moves1, Moves2, Moves12),
  append(Moves12, Moves3, Moves).

 
%======================= MOVES FOR COMPUTER EASY AND HARD ===========================================================================

% move_computer(+OldGameState, +GameState, -NewGameState, +Level)
% Gets a move for the computer based on the level of difficulty. In this case, easy level.
move_computer(OldGameState, GameState, NewGameState, 1) :-
  [OldBoard, _OldPlayer] = OldGameState,
  [Board, Player] = GameState,
  get_all_moves(Board, Player, Moves),
  random_member(Move, Moves),
  translate_move(Board, Move, NewBoard),
  OldBoard \= NewBoard,
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

% Gets a move for the computer based on the level of difficulty. In this case, hard level.
move_computer(OldGameState, GameState, NewGameState, 2) :-
  [OldBoard, _OldPlayer] = OldGameState,
  [Board, Player] = GameState,
  get_all_moves(Board, Player, Moves), !,
  minimax(OldBoard, Board, Player, Moves, List, 2, max),
  sort(List, SortedList),
  last(SortedList, Delta-_Nbd),
  get_lowest_elements(SortedList, Delta, LowestElements),
  random_member(_Delta2-NewBoard, LowestElements),
  change_player(Player, NewPlayer),
  NewGameState = [NewBoard, NewPlayer].

% Helper predicate to collect elements with the lowest value
get_lowest_elements([], _, []).
get_lowest_elements([Delta-Board|Rest], MinValue, LowestElements) :-
  Delta =:= MinValue,
  get_lowest_elements(Rest, MinValue, RestLowestElements),
  LowestElements = [Delta-Board | RestLowestElements].
get_lowest_elements([_|Rest], MinValue, LowestElements) :-
  get_lowest_elements(Rest, MinValue, LowestElements).


% minimax(+OldBoard, +Board, +Player, +Moves, -FinalList, +Value1, +Depth, +Type)
minimax(OldBoard, Board, Player, Moves, FinalList, Value1, 1, Type) :- 
  minimax(OldBoard, Board, Player, Moves, [], FinalList, Value1, 1, Type).

minimax(_OldBoard, _Board, _Player, [], Acc, Acc, _Value1, 1, _Type):- !.

minimax(OldBoard, Board, Player, Moves, Acc, FinalList, MaxValue, 1, Type) :-
  [CurrMove|T] = Moves,
  translate_move(Board, CurrMove, Board2),
  value(Board2, Player, Value2),
  max_or_min(Type, Value2, MinValue),
  Delta is MaxValue + MinValue,
  NewAcc = [Delta | Acc],
  minimax(OldBoard, Board, Player, T, NewAcc, FinalList, MaxValue, 1, Type).

minimax(OldBoard, Board, Player, Moves, FinalList, 2, Type) :- 
  minimax(OldBoard, Board, Player, Moves, [], FinalList, 2, Type).

minimax(_OldBoard, _Board, _Player, [], Acc, Acc, 2, _Type):- !.

minimax(OldBoard, Board, Player, Moves, Acc, FinalList, Depth, Type) :-
  [CurrMove|T] = Moves,
  translate_move(Board, CurrMove, Board1),
  value(Board1, Player, Value1),
  max_or_min(Type, Value1, MaxValue),
  change_player(Player, NewPlayer),
  get_all_moves(Board1, NewPlayer, Moves2),
  \+ Moves2 = [],
  NewDepth is Depth - 1,
  swap_min_max(Type, NewType),
  minimax(Board, Board1, NewPlayer, Moves2, List2, MaxValue, NewDepth, NewType),
  sort(List2, SortedList2),
  [Delta | _] = SortedList2,
  NewAcc = [Delta-Board1 | Acc],
  minimax(OldBoard, Board, Player, T, NewAcc, FinalList, Depth, Type).

swap_min_max(min, max).
swap_min_max(max, min).

max_or_min(min, Value, MinValue):- MinValue is -Value.
max_or_min(max, Value, Value).



%============================ GET BOARD VALUE ======================================================================

% value(+Board, +Player, -Value)
% Calculates the value of a board for a given player. Used for the hard level in computer mode.
value(Board, Player, Value) :-
  iterate_board(Board, XValue, OValue),
  (Player == player1 -> Value is XValue - OValue; Value is OValue - XValue).


% iterate_board(+Board, -XValue, -OValue)
% Iterates through the board and calculates the value of the board for each player based on tower heights and tops.
iterate_board(Board, FinalXValue, FinalOValue) :-
  RowIndex is 1,
  XValue is 0,
  OValue is 0,
  iterate_rows(Board, RowIndex, XValue, OValue, FinalXValue, FinalOValue).

% iterate_rows(+Board, +RowIndex, +XValue, +OValue, -FinalXValue, -FinalOValue)
% Iterates through the rows of the board and calculates the value of the board for each player based on tower heights and tops. Calls iterate_columns/6.
iterate_rows([], _, XValue, OValue, XValue, OValue).
iterate_rows([Row|Rest], RowIndex, XValue, OValue, FinalXValue, FinalOValue) :-
  iterate_columns(Row, RowIndex, 1, XValue, OValue, NewXValue, NewOValue),
  NextRow is RowIndex + 1,
  iterate_rows(Rest, NextRow, NewXValue, NewOValue, FinalXValue, FinalOValue).

% iterate_columns(+Row, +RowIndex, +ColIndex, +XValue, +OValue, -FinalXValue, -FinalOValue)
% Iterates through the columns of the board and calculates the value of the board for each player based on tower heights and tops, by calling process_cell/7.
iterate_columns([], _, _, XValue, OValue, XValue, OValue).
iterate_columns([Cell|Rest], RowIndex, ColIndex, XValue, OValue, FinalXValue, FinalOValue) :-
  process_cell(Cell, RowIndex, ColIndex, XValue, OValue, NewXValue, NewOValue),
  NextCol is ColIndex + 1,
  iterate_columns(Rest, RowIndex, NextCol, NewXValue, NewOValue, FinalXValue, FinalOValue).

% process_cell(+Cell, +RowIndex, +ColIndex, +XValue, +OValue, -NewXValue, -NewOValue)
% Calculates the value of the board for each player based on tower heights and tops.
process_cell(empty, _, _, XValue, OValue, XValue, OValue).

process_cell(Cell, _, _, XValue, OValue, NewXValue, NewOValue) :-
  length(Cell, TowerHeight),
  tower_top(Cell, Top),
  update_values(Top, TowerHeight, XValue, OValue, NewXValue, NewOValue).
                                    
% update_values(+Top, +TowerHeight, +XValue, +OValue, -NewXValue, -NewOValue)
% Updates the values based on the top of the tower and the tower height. If there is a king, the value is increased by 100.
update_values(x, TowerHeight, XValue, OValue, NewXValue, OValue) :-
  TowerHeight >= 6,
  NewXValue is XValue + (TowerHeight * TowerHeight) + 100.
update_values(x, TowerHeight, XValue, OValue, NewXValue, OValue) :-
  TowerHeight < 6,
  NewXValue is XValue + (TowerHeight * TowerHeight).
update_values(o, TowerHeight, XValue, OValue, XValue, NewOValue) :-
  TowerHeight >= 6,
  NewOValue is OValue + (TowerHeight * TowerHeight) + 100.
update_values(o, TowerHeight, XValue, OValue, XValue, NewOValue) :-
  TowerHeight < 6,
  NewOValue is OValue + (TowerHeight * TowerHeight).




%=============================== TRANSLATE MOVES INTO BOARDS ===================================================================

% translate_move(+Board, +Move, -NewBoard)
% Translates a move into a board, depending on the move type:
% Moves of type 1 (place pawn).
translate_move(Board, [1, Player, _X, _Y, NewX, NewY, _NPieces], NewBoard) :-
    place_pawn(Board, NewX, NewY, Player, NewBoard).
% Moves of type 2 (move tower).
translate_move(Board, [2, _Player, X, Y, NewX, NewY, _NPieces], NewBoard) :-
    move_tower(Board, X, Y, NewX, NewY, NewBoard).
% Moves of type 3 (separate tower).
translate_move(Board, [3, _Player, X, Y, NewX, NewY, NPieces], NewBoard) :-
    separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard).

test_translate_move :-
  Board = [
    [[x,o], [x], empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, [x,x], empty, empty],
    [[o,o], empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
  ],
  translate_move(Board, [3, player1, 1, 1, 2, 1, 2], NewBoard),
  length(NewBoard, Size),
  display_game(Size, NewBoard).