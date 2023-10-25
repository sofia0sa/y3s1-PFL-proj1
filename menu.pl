% FILE TO PRINT MENUS AND GET USER INPUT (at the beginning of the game)
% also with the use_modules necessary

:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).
:- consult(board).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(charsio)).


% =============== CHOOSE MAIN OPTION ========================== %
choose_main_option(GameMode) :-
    choose_number(1, 3, '\nType a number', Option), !,
    main_option(Option, GameMode).

main_option(1, GameMode):- 
    print_modes(GameMode).

main_option(2, GameMode) :-
    clear_console,
    write('  _______________________________________________________________________ \n'),
    write(' |       RULES:                                                          |\n'),
    write(' |                                                                       |\n'), 
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |_______________________________________________________________________|\n'),
    write(' \n Type 0 to go back: '),
    read_number(Value),
    (Value =:= 0 -> clear_console, print_main_menu(GameMode); main_option(2)).


main_option(3, GameMode):-
    clear_console,
    GameMode = 0,
    write('Sorry to see you go!!...\n\n'),
    write('  _______________________________________________________________________ \n'),
    write(' |       LEAVING                                                         |\n'),
    write(' |                                                                       |\n'), 
    write(' |                         ***                                           |\n'),
    write(' |                        *                                              |\n'),
    write(' |                        ****                                           |\n'),
    write(' |                        *   *                                          |\n'),
    write(' |                         *** MAKING ...                                |\n'),
    write(' |_______________________________________________________________________|\n'),
    fail.

% =============== CHOOSE GAME MODES ========================== %

% menu/0
% Main menu
choose_mode(GameMode) :-  
    choose_number(0, 3, '\nType a number', GameMode), !,
    mode_option(GameMode).

% option(+N)
% Main menu options. Each represents a game mode.
mode_option(1):-
    write('\nPlayer vs. Player\n\n'),
    get_name(player1), get_name(player2).

mode_option(2):-
    write('\nPlayer vs. Computer\n\n'),
    get_name(player1),
    asserta((name_of(player2, 'Computer'))), !, 
    choose_difficulty(player2).

mode_option(3):-
    write('Computer vs. Computer\n'),
    asserta((name_of(player1, 'Computer1'))),
    asserta((name_of(player2, 'Computer2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

mode_option(0):-
    clear_console,
    print_main_menu(GameMode).


% =============== CHOOSE PLAYER ========================== %

% choose_player(-Player)
% Unifies player with the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    % string_codes(Name1Str, Name1),
    % string_codes(Name2Str, Name2),
    %format('Who starts playing?\n1 - ~s being White player \n2 - ~s being Black player\n', [Name1Str, Name2Str]),
    format('\nWho starts playing?\n1 - ~s, with white pieces (uppercase) \n2 - ~s, with black pieces (lowercase) \n', [Name1, Name2]),
    choose_number(1, 2, '\nSelect', Index),
    nth1(Index, [player1, player2], Player).



% =============== CHOOSE DIFFICULTY ========================== %

% choose_difficulty(+Computer)
% Choose Computer difficulty (1 or 2)
choose_difficulty(Computer) :-
    format('\nPlease select ~a difficulty:\n', [Computer]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    choose_number(1, 2, '\nType a number', Option), !,
    asserta((difficulty(Computer, Option))).



% =============== CHOOSE BOARD ========================== %

% choose_board(-Size)
% Board size choice
choose_board(Size):-
    write('\nWhat board size do you want to choose: 4 or 5? '),
    repeat,
    read_number(Size),
    member(Size, [4, 5]), !. % Size must be some of the list 4 or 5


% =============== PRINTS ========================== %

% print_header/0
% Game header
print_header :-
    write('  _______________________________________________________________________ \n'),
    write(' |       WELCOME TO                                                      |\n'),
    write(' |                                                                       |\n'), 
    write(' |                         ***                                           |\n'),
    write(' |                        *                                              |\n'),
    write(' |                        ****                                           |\n'),
    write(' |                        *   *                                          |\n'),
    write(' |                         *** MAKING !!                                 |\n'),
    write(' |_______________________________________________________________________|\n\n').


print_main_menu(GameMode) :-
    write('  _______________________________________________________________________ \n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),                                                                                                                                            
    write(' |                      What do you want to do?                          |\n'),
    write(' |                                                                       |\n'),
    write(' |                      1. Choose a game mode                            |\n'),
    write(' |                                                                       |\n'),
    write(' |                      2. View game rules                               |\n'),
    write(' |                                                                       |\n'),
    write(' |                      3. Leave game                                    |\n'),
    write(' |                                                                       |\n'),
    write(' |_______________________________________________________________________|\n\n'),
    choose_main_option(GameMode).


print_modes(GameMode) :-
    clear_console,
    write('  _______________________________________________________________________ \n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),                                                                                                                                            
    write(' |                      Choose a Game Mode:                              |\n'),
    write(' |                                                                       |\n'),
    write(' |                      1. Player vs Player                              |\n'),
    write(' |                                                                       |\n'),
    write(' |                      2. Player vs Computer                            |\n'),
    write(' |                                                                       |\n'),
    write(' |                      3. Computer vs Computer                          |\n'),
    write(' |                                                                       |\n'),
    write(' |                      0. Go back to main menu                          |\n'),
    write(' |                                                                       |\n'),
    write(' |_______________________________________________________________________|\n\n'),
    choose_mode(GameMode).


% =============== MAIN MENU (called by play.) ========================== %

% main_menu(-GameState)
% Initialize GameState with Board, first Player
main_menu([Board,Player, GameMode]):-
    print_header,
    print_main_menu(GameMode),
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board),
    GameState = [Board, Player, GameMode],
    get_move(GameState, NewGameState).

% main_menu
% If user leaves the game, fail and exit
main_menu(_).