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
% print_turn(+Player)
% Prints a message declaring whose turn it is
print_turn(Player):-
    write('\n=========================================\n'),
    name_of(Player, Name),
    format('It`s ~a`s turn!\n', [Name]), !.
    % atom_string(NameAtom, Name)
    % format('It`s ~w`s turn!\n', [NameAtom]), !.


%===================== GAME HUMAN MOVES ====================

get_move(GameState, NewGameState) :-
    write('\n=========================================\n'),
    write('What move do you want to make?\n'),
    write('1 - Add piece\n'),
    write('2 - Move piece\n'),
    write('3 - Separate tower\n'),
    choose_number(1, 3, '\nType a number', Option), !,
    move_option(GameState, Option, NewGameState).

% check_if_tower_exists(+Board, +X, +Y, -L)
% Checks if there is a tower in the given coordinates to check if a player can separate it
check_if_tower_exists(Board, X, Y, L) :-
    get_tower(Board, X, Y, Tower),
    \+ empty_cell(Board, X, Y),
    length(Tower, L),
    L>1,
    !.

%checks if the player can place the tower in the given coordinates
% !DUVIDA: jogador so deve poder colocar em cima de peças dele, senao era facil ganhar (bastava ir somando ate uma torre = 6)?
check_if_can_place_tower(Board, X1, Y1, NPieces) :- 
    get_tower(Board, X1, Y1, Tower),
    length(Tower, L),
    L1 is L+NPieces,
    L1 =<6,
    !.

% move_option(+GameState, +Option, -NewGameState)
% Unifies NewGameState with the new game state after the player chooses an option
move_option(GameState, 1, NewGameState) :-
    [Board, Player, GameMode] = GameState,
    write('\n=========================================\n'),
    write('Where do you want to place the piece?\n\n'),
    repeat,
    get_coordinate(Board, X, Y),
    place_pawn(Board, X, Y, Player, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer, GameMode].

move_option(GameState, 2, NewGameState) :-
    [Board, Player, GameMode] = GameState,
    write('\n=========================================\n'),
    write('Which tower do you want to move?\n'),
    repeat,
    get_coordinate(Board, X, Y),
    \+ empty_cell(Board, X, Y),
    % get_tower(Board, X, Y, Piece),
    repeat,
    write('\nWhere do you want to move it?\n'),
    get_possible_moves(Board, X, Y, ListOfMoves, NMoves), %prints and lets choose the move
    %!TEST:
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    move_tower(Board, X, Y, NMove, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer, GameMode].


% IN CONSTRUCTION!
move_option(GameState, 3, NewGameState) :-
    [Board, Player, GameMode] = GameState,
    write('\n=========================================\n'),
    repeat,
    write('Which tower do you want to separate?\n'),
    get_coordinate(Board, X, Y),
    check_if_tower_exists(Board, X, Y, L), %checks if there is a tower to separate in the given coordinates

    repeat,
    write('\nHow many pieces do you want to move from the tower?\n'),
    L1 is L-1,
    choose_number(1, L1, 'Type a number', NPieces),

    repeat,
    write('\nWhere do you want to place them?\n'),
    get_possible_moves(Board, X, Y, NPieces, ListOfMoves, NMoves), %possible moves para que nao aconteca um placement com length>6
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    write('NMove: '), write(NMove), nl,
    separate_tower(Board, X, Y, NMove, NPieces, NewBoard),

    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer, GameMode].


get_possible_moves(Board, X, Y, ListOfMoves, L) :-
    write('HERE IN get_possible_moves'), nl,
    valid_moves(Board, X, Y, ListOfMoves),
    length(ListOfMoves, L),
    L>0,
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves).


    % get_coordinate(Board, X1, Y1),
    % move_pieces(Board, X, Y, X1, Y1, NewBoard),
    % print_board(Board, NewBoard).


% neste print, ja nao deve mostrar movimentos que coloquem em cima de torres que length daria >6 !!
% print_possible_moves(+Board, +X, +Y, +NPieces, +PlaceFlag)
get_possible_moves(Board, X, Y, NPieces, ListOfMoves, L) :-
    write('HERE IN get_possible_moves'), nl,
    valid_moves(Board, X, Y, ListOfMoves, NPieces),
    length(ListOfMoves, L),
    L>0,
    write('ListOfMoves: '), write(ListOfMoves), nl,
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves).




% get_coordinate(+Board,-X, -Y)
% Unifies Coordinate with a valid coordinate given by input within the Board
get_coordinate(Board, X, Y):-
    length(Board, Size),
    choose_number(1, Size, 'Choose column', X),
    choose_number(1, Size, 'Choose row', Y).


% ==================== GAME OVER ====================

% game_over(+GameState, +Winner)
% Checks if the game is over
% game_over([Board,OtherPlayer,_, _], Winner):-
%     check_winner(Board, Winner).
    % other_player(OtherPlayer, Winner).
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
show_winner(Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves), %isto não é nosso pois não?
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).



% ============================== GAME CYCLE ====================
% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):- %IF GAME IS OVER because someone won
    7 =:= 3, !,
    % game_over(Board, Winner), !, %verifica se alguem ganhou (length tower = 6)

    [Board, Player, GameMode] = GameState,
    
    length(Board, Size),
    print_board(Size, Board),

    show_winner(Winner).


game_cycle(GameState):- % HERE in case nobody is winning atm
    write('NEW GAME CYCLE\n'),
    [Board, Player, GameMode] = GameState, 
    length(Board, Size),
    print_board(Size, Board),
    print_turn(Player),
    get_move(GameState, NewGameState), %para player humano
    % move(GameState, Move, NewGameState), !,  %this was giving errors
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