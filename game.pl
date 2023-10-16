% DICHEIRO DE STARTING POINT -> aqui se começa o jogo com play.

:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').
:- consult('menu.pl'). %ficheiro com os includes
:- consult('utils.pl').

% FUNCTION THAT STARTS THE GAME!!
% play/0
% Starts the game and clears data when it ends 
play :-
    main_menu(GameState), !,
    game_cycle(GameState),
    clear_data.