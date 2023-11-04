
## REGRAS

[https://www.boardspace.net/sixmaking/english/Six-MaKING-rules-Eng-Ger-Fra-Ro-Hu.pdf]


## DISTRIBUIÇAO ENTRE FICHEIROS

Board -> funções para o tabuleiro, desenhar, translate e movimentos e alteraçoes de acordo com as jogadas
Menu -> opçoes de menu e flow
utils -> funçoes usadas muitas vezes


FABIO:
board -> desenha, coloca e tira peças
configurations -> desenha os menus e questiona o utilizador sobre as configs do jogo (nome, número de peças, modo)
data -> constantes, a tua base de factos inicial
utils -> funções auxiliares
main -> o jogo em si, avalia cada jogada e possui os elementos de AI se for esse o modo selecionado 
se fosse agora talvez teria feito em 4 ficheiros (MVCD), model, view, controller, data.


ONDE PODEMOS DAR STEP UP:
- prints bonitos no menu como o jogo da bruna
- dar nomes aos players como o fabio fez, ficou giro
- algoritmo qlqr mas isso é extra (minimax no trabalho deles)


CHATGPT
asserta/1 is a built-in predicate in Prolog that adds a new fact to the knowledge base as the first clause of the predicate. In the case of the get_name/1 predicate, asserta/1 is used to add a new fact to the knowledge base that associates the given Player with the Name that was entered by the user.
The asserta/1 predicate takes a single argument, which is a Prolog term representing the fact to be added to the knowledge base. In this case, the fact being added is name_of(Player, Name), which associates the given Player with the Name that was entered by the user.


_______________________________________________________

The last fact main_menu(_). is used to handle the case where the user leaves the game without making a selection from the main menu.

In Prolog, a predicate can have multiple clauses with different patterns of arguments. When a predicate is called, Prolog will try to match the arguments with the patterns of the clauses in the order they are defined. If a match is found, Prolog will execute the corresponding clause.

In the case of the main_menu/1 predicate, the first clause defines the behavior when the user selects an option from the main menu. The second clause defines the behavior when the user leaves the game without making a selection.

The second clause has a single underscore (_) as its argument, which is a wildcard that matches any value. This means that the second clause will match any argument, regardless of its value.

When the user leaves the game without making a selection, the main_menu/1 predicate will fail to match the argument with the first clause, and Prolog will continue searching for a matching clause. Since the second clause matches any argument, it will be selected as the next clause to execute.

The second clause does not perform any actions, but it prevents the program from crashing or producing unexpected behavior when the user leaves the game without making a selection. Instead, the program will simply fail and exit gracefully.

-----

1 - pawn (1 cell horizontal or vertical)
2 - rook (any number of cells horizontal or vertical, until finding another piece)
3 - knight (2 cells horizontal or vertical, then 1 cell in the other direction)
4 - bishop (any number of cells diagonal, until finding another piece)
5 - queen (any number of cells horizontal, vertical or diagonal, until finding another piece)
6 - king (wins) 



torres da forma 
o-o
x-x
o-x-o-x

P = _-_-_ -> verificar se peça tem tamanho 3
O win state é verificar se há uma torre de 6 de altura, e qual a cor no topo


B -> board
|
NB -> new board
|
NNB -> new new board

guardar sempre o board deste momento e o board anterior

| P1 | P1 | Board |
| --- | --- | --- |
| x   | | NB |
| | o| NNB |
| x   | | NB |
| | o| NNB |


---
replace(B,X,Y,O,H,NB)
change_human(H,NH)


---


DUVIDAS PARA AULA:

- game cycle, ver se esta bem, e ver como lidar com as varias formas de jogo aqui
- com as varias formas de jogo, como fazer as funcoes? dar nomes diferentes? nao reutilizasmos nada da primeira versao humanoVShumano?
e chamamos onde essas funcoes? temos que fazer um gamecyle para cada modo de jogo? se nao, como incorporamos no mesmo?

- duvidas do GUI em relacao ao movimento de algumas peças
--
- quando alguem ganha o jogo, podemos dar fail? e a pessoa diz play. para jogar outra vez?

NOTAS:

- nao esquecer de fazer repeat para error handling de inputs errados 

TO DO:
- error handling quando derem nomes vazios (ver se length>0) - MAYBE
- limpar writes (HERE IN)
- podemos separar torres cujo topo nao seja nosso? - SIM

- MOVE BISHOP
- AI HARD: 
aplicar à lista as conversoes de move para board, 
value() para avaliar boards obtidas, 
avaliar todas essas boards, 
para cada board calcular agora todos os moves possiveis para o outro jogador (depth=2), 
avaliar novamente as boards resultantes
ordenar a board do jogador atual ?

BUGS:

- menu principal, se dermos input incorreto aquilo da print 2x do "Type a number between" -> é suposto so dar 1!!
- quando damos letras no choose_number, ele passa-se e printa 3 vezes type a number
- PROF daniel => Perguntar se o nosso algoritmo é considerado um minimax, ou se é um algoritmo qlqr
- PROF daniel => HA MELHOR MANEIRA DE FAZER SAIR DO JOGO? EU FIZ HALT E SAI DO SICSTUS, MAS QUERIA QUE PROLOG RETORNASSE 'YES'

TO DO:
- Relembrar ao utilizador se joga com maiúsculas ou minúsculas
- KO rule
- Melhorar Value 
prof:
- fazer um random select das jogadas IA possiveis quando o valor é igual -> pode levar a que o jogo entre duas IA hard seja diferente e nao sempre igual
- o abort funcionou para sair do jogo (seria melhor com yes mas nao é dito no enunciado. seria mover o resto do predicado main para apos o choose game mode = 2)
- devemos ter 3 gamestates para evitar loops -> fazer branch e testar se nao parte o jogo -> NO FIM!
- colocar o predicado minimax com a recursao (por poucas decimas)
- vitoria da mais +100 pontos no value para ser evidente que um dado jogador ganharia com essa jogada

FILES DOCUMENTADOS:
- menu.pl
- game.pl
- utils.pl


FALTA:
- board.pl
- game_logic.pl
- game_logic_computer.pl

The second clause does not perform any actions, but it prevents the program from crashing or producing unexpected behavior when the user leaves the game without making a selection. Instead, the program will simply fail and exit gracefully

LIMPOS (revisto no final):

SE NÃO DER MINIMAX:
- computador faz algo +/- inteligente: procura a torre mais alta, ve se existe alguma torre que possa separar e mover para cima dessa torre, desde que o topo seja do proprio computador


---- LISTA ESTRUTURA MOVE PARA A GERAÇÃO DE TODAS AS MOVES POSSIVEIS ----

[MoveFlag, Player, X, Y, NewX, NewY, NPieces]
-> trans

MoveFlag : 1 (add), 2 (move), 3 (separate)-> tipo de move escolhido no menu
NPieces : -1 (add e move), 1 (separate) -> numero de peças a mover
X e Y : 0 (add piece) e coordenadas atuais (move e separate)
NewX e NewY : coordenadas novas (add, move e separate)

---- 
- Criar Moves to tipo 1
- 



------ REGRAS (texto corrido) ------

Six MaKING
Author: Dorsonczky József
© 2o13 – Dorsonczky József
© 2o13 – Mind Fitness Games (OVECo S.R.L.)

Accessories: 1 game board, 32 wooden disks (24
in case of 4x4 game board), 1 small bag for disks, 1
rulebook.
Like in Makarenko chess, the towers are built using
the wooden disks, and regarding their height, each
one represents a chess piece: 1 disk Pawn, 2 Rook, 3
Knight, 4 Bishop, 5 Queen, 6 or more King

Goal of the game
Building a King using six or more disks with your
own color on top.

Gameplay
Six MaKING is played on a 5x5 board, where each
player holds 16 wooden disks (in case of a 4x4 board
each player holds 12 disks). Players choose the color
they play with and decide on the starting order.
The game starts with an empty board, and players
alternate turns. The player whose turn is, may choose
whether to:

l place a new disk (a Pawn) on the board;
l or move one of the towers or a part of a tower.
I., Placing a new disk on the board
A player may place a single disk (a Pawn) on any
empty square of the board.

II., Moving the entire or a part of a tower
 1 A player may move the towers on the board
regarding the movements of the corresponding chess
pieces. A tower can only be moved on top of another,
but not on empty squares (see Fig. 3.):
1 disk (Pawn) – moves a single square in all four
directions on top of an adjoining tower.
2 disks (Rook) – moves any number of squares
orthogonally on the first tower in its path.
3 disks (Knight) – moves in L shape on top of
another tower.
4 disks (Bishop) – moves any number of squares
diagonally on the first tower in its path.
5 disks (Queen) – moves any number of squares in
all eight directions on the first tower in its path.
2 A player may move the entire tower, or only a part
of it. In case of moving only a part of a tower, the disks
are always taken from the top, and they move as the
original tower, before being split (see Fig. 3.).
 3 A player may move with towers of both colors.
The topmost disk of a tower only indicates the color of
it, but not the player who controls it.
 4 Ko-rule: movements that lead to the same
situation that was two turns before are considered
illegal, that means undoing the opponents move is not
allowed.

End of the game
The game ends when the first tower consisting of six
or more disks (King) is built. The player who has its
color on top is the winner.
There is a more complex version, called
Six MaKING, where every time a King is built, the
tower is eliminated from the board and the disks that
formed it are redistributed among the players. The
player who had its color on top gets one winning
point. The first player reaching 6 winning points wins
the game.