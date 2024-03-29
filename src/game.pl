:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').
:- consult('menu.pl'). 
:- consult('utils.pl').
:- consult('board.pl').

% ===================== PRINT NEXT PLAYER ==================

% print_turn(+Player)
% Prints message indicating the next player to play.
print_turn(Player):-
    write('\n=========================================\n'),
    name_of(Player, Name),
    player_case(Player, Case), 
    format('\nIt`s ~a`s turn!(~s)\n', [Name, Case]), !.

%===================== GAME HUMAN MOVES ====================

% get_move(+OldGameState, +GameState, -NewGameState)
% Prints choice of moves in case it´s a human player or calls the move predicate in case it´s the Computer. 
% Predicate for printing and choosing a possible movement in case it's a human player.
get_move(OldGameState, GameState, NewGameState) :-
    [_Board, Player] = GameState,
    \+difficulty(Player, _), !, 
    repeat,
    write('\n=========================================\n'),
    write('\nWhat move do you want to make?\n'),
    write('1 - Add pawn\n'),
    write('2 - Move tower\n'),
    write('3 - Separate tower\n'),
    choose_number(1, 3, '\nType a number', Option), %!,
    move_option(OldGameState, GameState, Option, NewGameState).

% Predicate for choice of movement in case it's Easy Computer mode.
get_move(OldGameState, GameState, NewGameState) :- 
    [_Board, Player] = GameState,
    difficulty(Player, 1), !,
    move_computer(OldGameState, GameState, NewGameState, 1).

% Predicate for choice of movement in case it's Hard Computer mode.
get_move(OldGameState, GameState, NewGameState) :- 
    move_computer(OldGameState, GameState, NewGameState, 2).


% check_if_tower_exists(+Board, +X, +Y, -L)
% Checks if there is a tower in the given coordinates.
check_if_tower_exists(Board, X, Y, L) :-
    get_tower(Board, X, Y, Tower),
    \+ empty_cell(Board, X, Y),
    length(Tower, L),
    L>1,
    !.

% move_option(+OldGameState, +GameState, +Option, -NewGameState)
% Choice of move option.
% Choice: "Add pawn" option. Asks where to place the piece and places it in the given coordinates, if possible. Changes player after the move was made.
move_option(_OldGameState, GameState, 1, NewGameState) :-
    [Board, Player] = GameState,
    write('\n=========================================\n'),
    write('\nWhere do you want to place the pawn?\n\n'),
    get_coordinate(Board, X, Y),
    place_pawn(Board, X, Y, Player, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].

% Choice: "Move tower" option. Asks which tower to move and where to move. Moves it to the given coordinates, if possible. Changes player after the move was made.
move_option(_OldGameState, GameState, 2, NewGameState) :-
    [Board, Player] = GameState,
    write('\n=========================================\n'), 

    write('\nWhich tower do you want to move?\n'),
    get_coordinate(Board, X, Y),
    (empty_cell(Board, X, Y) -> 
        format('There is not a tower in position [~w,~w]!\n', [X, Y]),
        fail;
        true),
    write('\nWhere do you want to place it?\n'),
    get_possible_moves(Board, Player, X, Y, ListOfMoves, NMoves), 
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    [NewX, NewY] = NMove,
    move_tower(Board, X, Y, NewX, NewY, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].


% Choice: "Separate tower" option. Asks which tower and how many pieces wants to separate and where to move them. Separates the tower in the given coordinates, if possible. Changes player after the move was made.
move_option(OldGameState, GameState, 3, NewGameState) :-
    [Board, Player] = GameState,
    [OldBoard, _OldPlayer] = OldGameState,
    write('\n=========================================\n'), 
    
    write('\nWhich tower do you want to separate?\n'),
    get_coordinate(Board, X, Y),
    get_tower(Board, X, Y, Tower),
    (Tower == empty -> 
        format('There is not a tower in position [~w,~w]!\n', [X, Y]),
        fail;
        true),
    length(Tower, L),
    (L==1 -> 
        write('You can`t separate a pawn!\n'),
        fail;
        true),
    print_tower_structure(Tower, L),
    
    write('\nHow many disks do you want to move from the tower?\n'),
    L1 is L-1,
    choose_number(1, L1, 'Type a number', NPieces),

    write('\nWhere do you want to place them?\n'),
    get_possible_moves(Board, Player, X, Y, NPieces, ListOfMoves, NMoves), 
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    [NewX, NewY] = NMove,
    separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard),
    (OldBoard == NewBoard -> 
        write('\nWarning! You cannot undo your opponent`s move!\n'),
        !,
        fail;
        true),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].


% get_possible_moves(+Board, +Player, +X, +Y, -ListOfMoves, -L)
% Determines and prints the possible moves that a player can make in case of "Move tower" and "Separate tower" options.
% Possible moves for the "Move tower" option.
get_possible_moves(Board, Player, X, Y, ListOfMoves, L) :-
    valid_moves(Board, Player, X, Y, ListOfMoves),
    length(ListOfMoves, L),
    ( L>0 -> 
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves);
    format('No possible moves for the tower in [~d, ~d]!\n', [X, Y]),
    fail).

% Possible moves for the "Separate tower" option.
get_possible_moves(Board, Player, X, Y, NPieces, ListOfMoves, L) :-
    valid_moves(Board, Player, X, Y, ListOfMoves, NPieces),
    length(ListOfMoves, L),
    ( L>0 ->
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves);
    format('It`s not possible to move ~d disks of the tower in [~d, ~d]!\n', [NPieces, X, Y]),
    fail).

% get_coordinate(+Board, -X, -Y)
% Lets player choose coordinates based on the size of the board.
get_coordinate(Board, X, Y):-
    length(Board, Size),
    choose_number(1, Size, 'Choose column', X),
    choose_number(1, Size, 'Choose row', Y).


% ==================== GAME OVER ====================

% game_over(+Board, -Winner)
% Checks if there is any game winner.
game_over(Board, Winner):-
    check_winner(Board, Top),
    top_to_player(Top, Winner).

% ==================== GAME WINNER ====================

% show_winner(+Winner)
% Prints the winner of the game.
show_winner(Winner):-
    name_of(Winner, Name),
    format('Winner is ~a! Congrats!!\n', [Name]).

% ==================== GAME CYCLE ====================

% game_cycle(+OldGameState, +GameState)
% Loop that keeps the game running and checks if the game is over. If it´s not, calls the get_move predicate to get the next move.
% Checks if game is over. If it is, prints a winning message.
game_cycle(_OldGameState, GameState):-
    [Board, _Player] = GameState,
    game_over(Board, Winner), !, 
    display_game(Board),
    write('GAME OVER\n'), nl,
    show_winner(Winner).

% Calls recursively the get_move predicate to get the next move, while there is no winner, changing players' turns.
game_cycle(OldGameState, GameState):-
    [Board, Player] = GameState, 
    display_game(Board),
    print_turn(Player),
    get_move(OldGameState, GameState, NewGameState), 
    game_cycle(GameState, NewGameState).


% ==================== GAME START ====================

% play/0
% Starts the game, calls main predicate to save configurations, keeps game running in a game cycle and clears data when it ends. 
play :-
    clear_console,
    main(GameState), !,
    game_cycle(GameState, GameState),
    clear_data.