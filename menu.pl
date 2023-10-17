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


% choose_difficulty(+Computer)
% Choose Computer difficulty (1 or 2)
choose_difficulty(Computer) :-
    format('Please select ~a status:\n', [Computer]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Computer, Option))).

% option(+N)
% Main menu options. Each represents a game mode.
option(1):-
    write('Player vs. Player\n'),
    get_name(player1), get_name(player2).

option(2):-
    write('Player vs. Computer\n'),
    get_name(player1),
    asserta((name_of(player2, 'Computer'))), !, 
    choose_difficulty(player2).

option(3):-
    write('Computer vs. Computer\n'),
    asserta((name_of(player1, 'Computer1'))),
    asserta((name_of(player2, 'Computer2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% BUF ==> nao aceita opçao 0 no menu inicial
option(0):-
    write('Sorry to see you go!!...\n\n'),
    write('  _______________________________________________________________________ \n'),
    write(' |       LEAVING                                                         |\n'),
    write(' |                                                                       |\n'), 
    write(' |                         ***                                           |\n'),
    write(' |                        *                                              |\n'),
    write(' |                        ****                                           |\n'),
    write(' |                        *   *                                          |\n'),
    write(' |                         *** MAKING ...                                |\n'),
    write('  _______________________________________________________________________ \n').



% choose_player(-Player)
% Unifies player with the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    % string_codes(Name1Str, Name1),
    % string_codes(Name2Str, Name2),
    %format('Who starts playing?\n1 - ~s being White player \n2 - ~s being Black player\n', [Name1Str, Name2Str]),
    format('Who starts playing?\n1 - ~s being White player \n2 - ~s being Black player\n', [Name1, Name2]),
    choose_number(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).


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
    write('  _______________________________________________________________________ \n').


% menu/0
% Main menu
choose_mode :-  
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
    write(' |                      0. Leave Game                                    |\n'),
    write(' |_______________________________________________________________________|\n'),

    choose_number(0, 3, 'Type a number', Option), !,
    option(Option).


% choose_board(-Size)
% Board size choice
choose_board(Size):-
    write('Board size: 4 or 5?'),
    repeat,
    read_number(Size),
    member(Size, [4, 5]), !. % Size must be some of the list 4 or 5

% main_menu(-GameState)
% Initialize GameState with Board, first Player, empty FearList and TotalMoves
main_menu([Board,Player,[],0]):-
    print_header,
    choose_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).