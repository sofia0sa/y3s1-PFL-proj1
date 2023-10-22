% DICHEIRO DE STARTING POINT -> aqui se começa o jogo com play.

:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').
:- consult('menu.pl'). %ficheiro com os includes
:- consult('utils.pl').


% ==================== GAME MOVES -> retirado do FS, ver como fica o nosso ====================
% choose_move(+GameState,+Player,+Level,-Move)
% Choose move a human player
% choose_move([Board,Player,ForcedMoves,TotalMoves], ColI-RowI-ColF-RowF):-
%     \+difficulty(Player, _),                    
%     repeat,
%     get_move(Board, ColI-RowI-ColF-RowF),                 
%     check_forced_moves(ColI-RowI,ForcedMoves),
%     validate_move([Board,Player,ForcedMoves,TotalMoves], ColI-RowI, ColF-RowF), !.  
% choose_move([Board,Player,ForcedMoves,TotalMoves], Move):-
%     difficulty(Player, Level),                  
%     choose_move([Board,Player,ForcedMoves,TotalMoves], Player, Level, Move), !.   

% % choose_move(+GameState,+Player,+Level,-Move)
% % Bot random player. Makes a list of possible moves and select a random one
% choose_move(GameState, Player, 1, ColI-RowI-ColF-RowF):- 
%     valid_moves(GameState, Player, ListOfMoves),
%     random_member(ColI-RowI-ColF-RowF, ListOfMoves).

% % choose_move(+GameState,+Player,+Level,-Move)
% % Bot greedy player. Makes a list of possible moves and select the one with the most points according minimax algorithm
% choose_move(GameState, Player, 2, ColI-RowI-ColF-RowF):-
% 	valid_moves(GameState, Player, ListOfMoves),
%     other_player(Player, NewPlayer),
% 	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
%                                 move(GameState, Coordinate, NewGameState), 
%                                 value(NewGameState,Player, Value1),
%                                 minimax(NewGameState, NewPlayer, min, 1, Value2),
%                                 Value is Value1 + Value2), Pairs),
%     sort(Pairs, SortedPairs),
%     last(SortedPairs, Max-_),
%     findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
%     random_member(ColI-RowI-ColF-RowF, MaxCoordinates).



% ================================== PRINT NEXT PLAYER =================================================================
% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn(Player):-
    name_of(Player, Name),
    format('It`s ~a`s turn!\n', [Name]), !.


%===================== GAME HUMAN MOVES ====================

get_move(GameState, NewGameState) :-
    [Board, Player] = GameState,
    print_turn(Player),
    
    write('==============================\n'),
    write('What move do you want to make?\n'),
    write('1 - Add piece\n'),
    write('2 - Move piece\n'),
    write('3 - Separate tower\n'),
    choose_number(1, 3, '\nType a number', Option), !,

    move_option(GameState, Option, NewGameState).
    move(GameState, Coordinate, NewGameState).

check_if_tower_exists(Board, X, Y) :-
    get_piece(Board, X, Y, Piece),
    length(Piece, L),
    L>1,
    !.

% move_option(+GameState, +Option, -NewGameState)
% Unifies NewGameState with the new game state after the player chooses an option
move_option(GameState, 1, NewGameState) :-
    [Board, Player] = GameState,
    write('\n==============================\n'),
    write('Where do you want to place the piece?\n'),
    get_coordinate(Board, X, Y),
    place_pawn(Board, X, Y, Player, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].

move_option(GameState, 2, NewGameState) :-
    write('\n==============================\n'),
    write('Which piece do you want to move?\n'),
    get_coordinate(Board, X, Y),
    get_piece(Board, X, Y, Piece),
    print_possible_moves(Board, X, Y), %prints and lets choose the move
    move_piece(Board, X, Y, X1, Y1, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].

move_option(GameState, 3, NewGameState) :-
    write('\n==============================\n'),
    write('Which tower do you want to separate?\n'),
    get_coordinate(Board, X, Y),
    check_if_tower_exists(Board, X, Y), %
    separate_tower(Board, X, Y, Player, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].

% get_coordinate(+Board,-X, -Y)
% Unifies Coordinate with a valid coordinate given by input within the Board
get_coordinate(Board, X, Y):-
    length(Board, Size),
    choose_number(1, Size, 'Choose column', X),
    choose_number(1, Size, 'Choose row', Y).


% ==================== GAME OVER ====================

% game_over(+GameState, +Winner)
% Checks if the game is over
game_over([Board,OtherPlayer,_, _], Winner):-
    other_player(OtherPlayer, Winner).
    %funcoes para verificar ganhar ou perder


% ==================== GAME WINNER - VER COMO FAZER ====================
% Predicate of the player vs player game mode loop (player 1 won)
game(FinalGamestate,N,FinalGamestate) :-
    isEven(N),
    % game_over(1),
    write('\nCongrats, player 1 won!\n').

% Predicate of the player vs player game mode loop (player 2 won)
game(FinalGamestate,N,FinalGamestate) :-
    \+isEven(N),
    % game_over(2),

    write('\nCongrats, player 2 won!\n').


% show_winner(+GameState, +Winner)
% Prints the winner of the game and number of moves they made
show_winner([_,_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).



% ============================== GAME CYCLE ====================
% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):- %IF GAME IS OVER because someone won
    game_over(GameState, Winner), !, %verifica se alguem ganhou (lenght tower = 6)

    [Board, Player] = GameState,
    length(Board, Size),

    print_board(Board, Size),
    show_winner(GameState, Winner).


game_cycle(GameState):- % HERE in case nobody is winning atm

    [Board, _] = GameState, 
    length(Board, Size),

    print_board(GameState),
    print_turn(Player),
    get_move(GameState, Move), %para player humano
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).



% ==================== GAME START ====================


% FUNCTION THAT STARTS THE GAME and keeps the game playing!!
% play/0
% Starts the game and clears data when it ends 
play :-
    clear_console,
    main_menu(GameState), !,
    game_cycle(GameState),
    clear_data.