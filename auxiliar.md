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



estado final 
percorrer peças todas e ver se tem algum com tamanho 6


---
replace(B,X,Y,O,H,NB)
change_human(H,NH)


---


DUVIDAS PARA AULA:

- game cycle, ver se esta bem, e ver como lidar com as varias formas de jogo aqui
- com as varias formas de jogo, como fazer as funcoes? dar nomes diferentes? nao reutilizasmos nada da primeira versao humanoVShumano?
e chamamos onde essas funcoes? temos que fazer um gamecyle para cada modo de jogo? se nao, como incorporamos no mesmo?

- duvidas do GUI em relacao ao movimento de algumas peças

NOTAS:

- nao esquecer de fazer repeat para error handling de inputs errados 