% DICHEIRO DE STARTING POINT -> aqui se começa o jogo com play.

:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').
:- consult('menu.pl'). %ficheiro com os includes
:- consult('utils.pl').


%===================== GAME MOMENTS ====================

% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn([_, Player, _, _]):-
    name_of(Player, Name),
    format('It`s ~a`s turn!\n', [Name]), !.


get_move(GameState, NewGameState) :-
    print_turn(Player),
    
   % write('==============================\n'),
    %write('                              \n'),
    write('What move do you want to make?\n'),
    write('1 - Add piece\n'),
    write('2 - Move piece\n'),
    write('3 - Separate tower\n'),
    choose_number(1, 3, '\nType a number', Option), !,

    move_option(GameState, Option, NewGameState).
    move(GameState, Coordinate, NewGameState).


% move_option(+Board, -X, -Y, -+Player, -NewBoard)
% Unifies NewGameState with the new game state after the player chooses an option
move_option(GameState, 1, NewGameState) :-
    write('Where do you want to place the piece?\n'),
    get_coordinate(GameState, X, Y),
    place_pawn(Board, X, Y, Player, NewBoard).

move_option(GameState, 2, NewGameState) :-
    write('Which piece do you want to move?\n'),
    get_coordinate(Board, X, Y),
    check_if_piece_exists(Board, X, Y),
    print_possible_moves(Board, X, Y), %prints and lets choose the move
    move_pawn(Board, X, Y, X1, Y1, Player, NewBoard).

% get_coordinate(+Board,-X, -Y)
% Unifies Coordinate with a valid coordinate given by input within the Board
get_coordinate(Board, X, Y):-
    length(Board, Size),
    choose_number(1, Size, 'Choose column', X),
    choose_number(1, Size, 'Choose row', Y).


% ==================== GAME WINNER ====================
% Predicate of the player vs player game mode loop (player 1 won)
game(FinalGamestate,N,FinalGamestate) :-
    isEven(N),
    game_over(1),
    write('\nCongrats, player 1 won!\n').

% Predicate of the player vs player game mode loop (player 2 won)
game(FinalGamestate,N,FinalGamestate) :-
    \+isEven(N),
    game_over(2),

    write('\nCongrats, player 2 won!\n').


% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).


% FUNCTION THAT STARTS THE GAME and keeps the game playing!!
% play/0
% Starts the game and clears data when it ends 
play :-
    main_menu(GameState), !,
    game_cycle(GameState),
    clear_data.