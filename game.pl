:- consult('game_logic.pl').
:- consult('game_logic_computer.pl').

% Predicate used for the user's turn. The user will choose which N pieces and in what direction
% they will move.
% +Gamestate -> Current board
% +Player
% -NewGamestate -> New Board
move(Gamestate,Player,NewGamestate) :-

    format('\nPLAYER ~d -\n',[Player]),

    get_direction(Dir),
    get_number_plays(Player,N),
    choose_pieces(Gamestate,Player,Dir,[],NewPieces,NewGamestate1,N),
    move_pieces(NewGamestate1,Player,NewPieces,NewGamestate,N),
    check_attacks(Player,NewPieces,N),

    write('---------------------------------------------------------\n'),skip_line,!.
