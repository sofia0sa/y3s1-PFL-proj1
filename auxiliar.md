DISTRIBUIÇAO ENTRE FICHEIROS

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


-----


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