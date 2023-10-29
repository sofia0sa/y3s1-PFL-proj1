% FILE TO PRINT MENUS AND GET USER INPUT (at the beginning of the game)
% also with the use_modules necessary

:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
% :- consult(data).
:- consult(board).
:- use_module(library(lists)).
:- use_module(library(random)).
% :- use_module(library(charsio)).


% =============== CHOOSE MAIN OPTION ========================== %
choose_main_option :-
    repeat,
    choose_number(1, 3, '\nType a number', Option), !,
    main_option(Option).

main_option(1):- 
    print_modes.

main_option(2) :-
    clear_console,
    write('  _______________________________________________________________________ \n'),
    write(' |       RULES:                                                          |\n'),
    write(' |                                                                       |\n'),
    write(' |   The goal of this game is building a King using 6 or more disks      |\n'),
    write(' |   with your own color in the top disk.                                |\n'),
    write(' |                                                                       |\n'),
    write(' |   Six MaKING is played on a 5x5 board, where each                     |\n'),
    write(' |   player holds 16 wooden disks - in case of a 4x4 board               |\n'),
    write(' |   each player holds 12 disks. Players choose the color                |\n'),
    write(' |   they play with and decide on the starting order.                    |\n'),
    write(' |   The game starts with an empty board, and players                    |\n'),
    write(' |   alternate turns. The player whose turn is, may choose               |\n'),
    write(' |   whether to:                                                         |\n'),
    write(' |                                                                       |\n'),
    write(' |   -> place a new disk (a Pawn) on the board;                          |\n'),
    write(' |   -> or move one of the towers or a part of a tower.                  |\n'),
    write(' |                                                                       |\n'),
    write(' |   For each number of disks in a board cell, the tower takes           |\n'),
    write(' |   different roles and can move differently.                           |\n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |  1  - pawn - 1 cell horizontal or vertical                            |\n'), 
    write(' |  2  - rook - any number of cells horizontal or vertical,              |\n'),
    write(' |       until finding another piece                                     |\n'),
    write(' |  3  - knight - 2 cells horizontal or vertical,                        |\n'),
    write(' |       then 1 cell horizontal or vertical                              |\n'),
    write(' |  4  - bishop - any number of cells diagonal,                          |\n'),
    write(' |       until finding another piece                                     |\n'),
    write(' |  5  - queen - any number of cells horizontal, vertical or             |\n'),
    write(' |       diagonal, until finding another piece                           |\n'),
    write(' |  6+ - king - wins                                                     |\n'),
    write(' |                                                                       |\n'),
    write(' |                                                                       |\n'),
    write(' |   For more detailed information, consult the link below.              |\n'),
    write(' |_______________________________________________________________________|\n'),
    write('\nhttp://www.boardspace.net/sixmaking/english/Six-MaKING-rules-Eng-Ger-Fra-Ro-Hu.pdf   \n'),
    repeat,
    write(' \n Type 1 to go back: '),
    read_number(Value),
    Value =:= 1,
    clear_console,
    print_main_menu. 
    % (Value =:= 0 -> clear_console, print_main_menu; main_option(2)).

% ==================== SHORT RULES BEFORE BOARD ====================

print_short_rules :-
    write('\n=========================================\n'),
    write('\nREMEMBER:\n'),
    write('   1  - pawn - 1 cell horizontal or vertical                 \n'),
    write('   2  - rook - any number of cells horizontal or vertical,   \n'),
    write('        until finding another piece                          \n'),
    write('   3  - knight - 2 cells horizontal or vertical,             \n'),
    write('        then 1 cell horizontal or vertical                   \n'),
    write('   4  - bishop - any number of cells diagonal,               \n'),
    write('        until finding another piece                          \n'),
    write('   5  - queen - any number of cells horizontal, vertical or  \n'),
    write('        diagonal, until finding another piece                \n'),
    write('   6+ - king - wins                                          \n\n').

main_option(3):-
    clear_console,
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
choose_mode :-  
    repeat,
    choose_number(1, 4, '\nType a number', GameMode), !,
    mode_option(GameMode).

% option(+N)
% Main menu options. Each represents a game mode.
mode_option(1):-
    clear_console,
    write('\n=========================================\n'),
    write('\nPlayer vs. Player\n\n'),
    get_name(player1), get_name(player2).

mode_option(2):-
    clear_console,
    write('\n=========================================\n'),
    write('\nPlayer vs. Computer\n\n'),
    get_name(player1),
    asserta((name_of(player2, 'Computer'))), !, 
    choose_difficulty(player2).

mode_option(3):-
    clear_console,
    write('\n=========================================\n'),
    write('Computer vs. Computer\n'),
    asserta((name_of(player1, 'Computer1'))),
    asserta((name_of(player2, 'Computer2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

mode_option(4):-
    clear_console,
    print_main_menu.


% =============== CHOOSE PLAYER ========================== %

% choose_player(-Player)
% Unifies player with the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    % string_codes(Name1Str, Name1),
    % string_codes(Name2Str, Name2),
    %format('Who starts playing?\n1 - ~s being White player \n2 - ~s being Black player\n', [Name1Str, Name2Str]),
    format('\nWho starts playing?\n1 - ~s, with white pieces (UPPERCASE) \n2 - ~s, with black pieces (lowercase) \n', [Name1, Name2]),
    repeat,
    choose_number(1, 2, '\nSelect', Index),
    nth1(Index, [player1, player2], Player).



% =============== CHOOSE DIFFICULTY ========================== %

% choose_difficulty(+Computer)
% Choose Computer difficulty (1 or 2)
choose_difficulty(Computer) :-
    format('\nPlease select ~a difficulty:\n', [Computer]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    repeat,
    choose_number(1, 2, '\nType a number', Option), !,
    asserta((difficulty(Computer, Option))).



% =============== CHOOSE BOARD ========================== %

% choose_board(-Size)
% Board size choice
choose_board(Size):-
    repeat,
    write('\nWhat board size do you want to choose: 4 or 5? '),
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


print_main_menu :-
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
    choose_main_option.


print_modes :-
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
    write(' |                      4. Go back to main menu                          |\n'),
    write(' |                                                                       |\n'),
    write(' |_______________________________________________________________________|\n\n'),
    choose_mode.



% =============== MAIN MENU (called by play.) ========================== %

% main_menu(-GameState)
% Initialize GameState with Board, first Player
% main_menu([Board, Player]):-
main_menu(NewGameState):-
    print_header,
    print_main_menu,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    clear_console,
    print_short_rules,
    init_state(Size, Board), %estado inicial da board
    GameState = [Board, Player],
    get_move(GameState, NewGameState).

% main_menu
% If user leaves the game, fail and exit
main_menu(_).