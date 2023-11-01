:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').
:- consult('menu.pl'). 
:- consult('utils.pl').

% ===================== PRINT NEXT PLAYER ==================
% print_turn(+Player)
% Prints message indicating the next player to play.
print_turn(Player):-
    write('\n=========================================\n'),
    name_of(Player, Name),
    format('\nIt`s ~a`s turn!\n', [Name]), !.


%===================== GAME HUMAN MOVES ====================

% get_move(+GameState, -NewGameState)
% Prints choice of moves in case of human player or calls the move predicate in case of Computer. 
% Predicate for printing and choosing a possible movement in case of human player.
get_move(GameState, NewGameState) :- % para o player humano escolher move
    [Board, Player] = GameState,
    \+difficulty(Player, _), !, %verifica se o player é humano
    repeat,
    write('\n=========================================\n'),
    write('\nWhat move do you want to make?\n'),
    write('1 - Add pawn\n'),
    write('2 - Move tower\n'),
    write('3 - Separate tower\n'),
    choose_number(1, 3, '\nType a number', Option), %!,
    move_option(GameState, Option, NewGameState).

% Predicate for choice of movement in case of Easy Computer mode.
get_move(GameState, NewGameState) :- 
    [Board, Player] = GameState,
    difficulty(Player, 1), !,
    move_computer(GameState, NewGameState, 1). 

% Predicate for choice of movement in case of Hard Computer mode.
get_move(GameState, NewGameState) :- 
    move_computer(GameState, NewGameState, 2).


% check_if_tower_exists(+Board, +X, +Y, -L)
% Checks if there is a tower in the given coordinates.
check_if_tower_exists(Board, X, Y, L) :-
    get_tower(Board, X, Y, Tower),
    \+ empty_cell(Board, X, Y),
    length(Tower, L),
    L>1,
    !.

% check_if_can_place_tower(+Board, +X, +Y, +NPieces)
% Checks if a tower can be placed in the given coordinates.
check_if_can_place_tower(Board, X1, Y1, NPieces) :- 
    get_tower(Board, X1, Y1, Tower),
    length(Tower, L),
    L1 is L+NPieces,
    % L1 =<6, %KING only with 6 pieces
    !.

% move_option(+GameState, +Option, -NewGameState)
% Choice of one of the three move options.
% Choice of "Add piece" option. Asks where to place the piece and places it in the given coordinates, if possible. Changes player after the move was made.
move_option(GameState, 1, NewGameState) :-
    [Board, Player] = GameState,
    write('\n=========================================\n'),
    write('\nWhere do you want to place the piece?\n\n'),
    get_coordinate(Board, X, Y),
    place_pawn(Board, X, Y, Player, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].

% Choice of "Move piece" option. Asks where to move the piece and moves it to the given coordinates, if possible. Changes player after the move was made.
move_option(GameState, 2, NewGameState) :-
    [Board, Player] = GameState,
    write('\n=========================================\n'), 

    write('\nWhich tower do you want to move?\n'),
    get_coordinate(Board, X, Y),
    % \+ empty_cell(Board, X, Y),
    (empty_cell(Board, X, Y) -> 
        format('There is not a tower in position [~w,~w]!\n', [X, Y]),
        fail;
        true),
    write('\nWhere do you want to place it?\n'),
    get_possible_moves(Board, Player, X, Y, ListOfMoves, NMoves), %prints and lets choose the move
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    [NewX, NewY] = NMove,
    move_tower(Board, X, Y, NewX, NewY, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].


% IN CONSTRUCTION!
move_option(GameState, 3, NewGameState) :-
    [Board, Player] = GameState,
    write('\n=========================================\n'),
    % write('\nType 1 to go back (just in case there is no option to move) \n or anything else to continue\n'),
    % read_number(Value),
    % (Value =:= 1 -> get_move(GameState, NewGameState); true),   
    
    write('\nWhich tower do you want to separate?\n'),
    get_coordinate(Board, X, Y),
    check_if_tower_exists(Board, X, Y, L), %checks if there is a tower to separate in the given coordinates

    write('\nHow many pieces do you want to move from the tower?\n'),
    L1 is L-1,
    choose_number(1, L1, 'Type a number', NPieces),

    write('\nWhere do you want to place them?\n'),
    get_possible_moves(Board, Player, X, Y, NPieces, ListOfMoves, NMoves), %possible moves para que nao aconteca um placement com length>6
    choose_number(1, NMoves, 'Type a number', N1),
    nth1(N1, ListOfMoves, NMove),
    write('NMove: '), write(NMove), nl,
    [NewX, NewY] = NMove,
    separate_tower(Board, X, Y, NewX, NewY, NPieces, NewBoard),
    change_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer].


get_possible_moves(Board, Player, X, Y, ListOfMoves, L) :-
    valid_moves(Board, Player, X, Y, ListOfMoves),
    write('HERE IN get_possible_moves'), nl,
    write('HERE Player: '), write(Player), nl,
    write('HERE ListOfMoves: '), write(ListOfMoves), nl,
    length(ListOfMoves, L),
    ( L>0 -> 
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves);
    format('No possible moves for the tower in [~d, ~d]!\n', [X, Y]),
    fail).


% print_possible_moves(+Board, +X, +Y, +NPieces, +PlaceFlag)
get_possible_moves(Board, Player, X, Y, NPieces, ListOfMoves, L) :-
    write('HERE IN get_possible_moves'), nl,
    write('HERE Player: '), write(Player), nl,
    valid_moves(Board, Player, X, Y, ListOfMoves, NPieces),
    length(ListOfMoves, L),
    ( L>0 ->
    write('ListOfMoves: '), write(ListOfMoves), nl,
    write('Structure: [X,Y]\n'),
    print_list(ListOfMoves);
    format('It`s not possible to move ~d pieces of the tower in [~d, ~d]!\n', [NPieces, X, Y]),
    fail).




% get_coordinate(+Board,-X, -Y)
% Choice of coordinate based on choice of column and row number.
get_coordinate(Board, X, Y):-
    length(Board, Size),
    choose_number(1, Size, 'Choose column', X),
    choose_number(1, Size, 'Choose row', Y).



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
    format('Winner is ~a! Congrats!!\n', [Name]).
    % fail.



% ============================== GAME CYCLE ====================
% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):- %IF GAME IS OVER because someone won
    % 7 =:= 3, !,
    [Board, Player] = GameState,
    game_over(Board, Winner), !, %verifica se alguem ganhou (length tower = 6)
    write('GAME OVER\n'), nl,
    
    length(Board, Size),
    print_board(Size, Board),

    show_winner(Winner).


game_cycle(GameState):- % HERE in case nobody is winning atm
    write('NEW GAME CYCLE\n'),
    [Board, Player] = GameState, 
    % difficulty(Player, Dif),
    % write('Dif: '), write(Dif), nl,
    length(Board, Size),
    print_board(Size, Board),
    print_turn(Player),
    get_move(GameState, NewGameState), %para player humano
    % move(GameState, Move, NewGameState), !,  %this was giving errors
    game_cycle(NewGameState).



% ==================== GAME START ====================

% play/0
% Starts the game, calls configuration predicate (the main(-GameState) predicate), keeps game running in a game cycle and clears data when it ends. 
play :-
    clear_console,
    main(GameState), !,
    game_cycle(GameState),
    clear_data.