:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(board).
:- use_module(library(lists)).
:- use_module(library(random)).

% =============== MAIN MENU OPTIONS ========================== %

% choose_main_option/0
% Choice of main menu option.
choose_main_option :-
    repeat,
    choose_number(1, 3, '\nType a number', Option), !,
    main_option(Option).

% option(+N)
% Main menu options.
% Option to choose game mode.
main_option(1):- 
    print_modes.

% Option to view game rules.
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

% Option to leave the game.
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
    % trace,
    halt.
    % fail.
% ==================== SHORT RULES BEFORE BOARD ====================

% print_short_rules/0
% Prints short set of game rules and it's present before the first board is printed.
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
    write('   6+ - king - wins                                          \n'),
    write('\nThese rules will stay here.\n\n').


% =============== GAME MODES OPTIONS ========================== %

% choose_mode/0
% Choice of game mode.
choose_mode :-  
    repeat,
    choose_number(1, 4, '\nType a number', GameMode), !,
    mode_option(GameMode).

% option(+N)
% Choice of game mode and choice of difficulty in case computer modes were chosen.
% Game mode option: Player vs Player.
mode_option(1):-
    clear_console,
    write('\n=========================================\n'),
    write('\n       Player vs. Player\n\n'),
    get_name(player1), get_name(player2).

% Game mode option: Player vs Computer.
mode_option(2):-
    clear_console,
    write('\n=========================================\n'),
    write('\n       Player vs. Computer\n\n'),
    get_name(player1),
    asserta((name_of(player2, 'Computer'))), !, 
    choose_difficulty(player2).

% Game mode option: Computer vs Computer.
mode_option(3):-
    clear_console,
    write('\n=========================================\n'),
    write('     Computer vs. Computer\n'),
    asserta((name_of(player1, 'Computer1'))),
    asserta((name_of(player2, 'Computer2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% Extra option: Go back to main menu.
mode_option(4):-
    clear_console,
    print_main_menu.


% =============== CHOOSE PLAYER ========================== %

% choose_player(-Player)
% Choice of first player to play.
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('\nWho starts playing?\n1 - ~s, with white pieces (UPPERCASE) \n2 - ~s, with black pieces (lowercase) \n', [Name1, Name2]),
    repeat,
    choose_number(1, 2, '\nSelect', Index),
    nth1(Index, [player1, player2], Player).



% =============== CHOOSE DIFFICULTY ========================== %

% choose_difficulty(+Computer)
% Choice of Computer difficulty: Easy or Hard.
choose_difficulty(Computer) :-
    format('\nPlease select ~a difficulty:\n', [Computer]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    repeat,
    choose_number(1, 2, '\nType a number', Option), !,
    asserta((difficulty(Computer, Option))).



% =============== CHOOSE BOARD ========================== %

% choose_board(-Size)
% Choice of board size: 4x4 or 5x5.
choose_board(Size):-
    repeat,
    write('\nWhat board size do you want to choose: 4 or 5? '),
    read_number(Size),
    member(Size, [4, 5]), !. 


% =============== PRINTS ========================== %

% print_header/0
% Prints game header.
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

% print_main_menu/0
% Prints main menu.
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

% print_modes/0
% Prints game modes.
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

% main(-NewGameState)
% Calls configuration predicates and starts the game.
main(NewGameState):-
    print_header,
    print_main_menu,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    clear_console,
    print_short_rules,
    initial_state(Size, Board), %estado inicial da board
    GameState = [Board, Player],
    get_move(GameState, NewGameState).

% Used when game is exited.
% main(_).