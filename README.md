-> nota para escrever algures no relatorio : os nossos "pieces" no codigo correspondem aos discos

# SixMaking
## Grupo: SixMaking_7
Guilherme Brandão Monteiro (up202108668)
Sofia Resende Ferreira de Sá (up202108676)

## Instalação e Execução

Para executar o jogo, é necessário instalar o **SICStus Prolog** (de preferência, a versão 4.8.0), fazer *download* dos ficheiros presentes em **PFL_TP1_T06_SixMaking_7.zip** e descompactá-los. Dentro do diretório **src**, deve consultar o ficheiro **game.pl**, através de [File] -> [Consult] -> [game.pl] ou diretamente da linha de comandos do SICStus Prolog. Por fim, para iniciar o jogo, basta executar o comando **``play.``**. O jogo está disponível em ambientes Windows e Linux.

Para a possibilidade de observar todo o jogo sem limpeza permatura da consola do SICStus, recomendamos aumentar o número de linhas da consola, por exemplo, para 1000, através de [Settings] -> [Window Settings] -> [Save lines] -> [1000]. Esta alteração é de maior importância para a observação de um jogo entre dois computadores. Desta forma, as pequenas regras impressas antes de cada jogo ficarão visíveis durante mais tempo também.

## Descrição do Jogo

O jogo Six MaKING é uma criação de Dorsonczky József, e foi publicado em 2013 pela Mind Fitness Games (OVECo S.R.L.). É uma variação interessante do xadrez, no qual torres são construídas utilizando discos de madeira. Cada disco representa uma peça de xadrez, com o valor de: 1 disco para o Peão, 2 para a Torre, 3 para o Cavalo, 4 para o Bispo, 5 para a Rainha, e 6 ou mais para o Rei.

O objetivo do jogo é construir um Rei utilizando seis ou mais discos da sua cor no topo da torre.

O jogo é jogado em um tabuleiro de 5x5 ou 4x4, dependendo da versão escolhida, e cada jogador possui 16 discos de madeira ou 12 na versão 4x4. Os jogadores escolhem a cor com a qual desejam jogar e definem a ordem inicial.

O jogo começa com o tabuleiro vazio, e os jogadores alternam as suas jogadas. Cada jogador, na sua vez, pode escolher entre:

- Colocar um novo disco (um Peão) no tabuleiro;
- Mover uma torre completa ou parte dela.

Para colocar um novo disco, o jogador pode escolher qualquer célula vazia do tabuleiro.

Para mover uma torre ou parte dela, o jogador segue os movimentos correspondentes às peças do xadrez. Por exemplo, uma torre composta por 2 discos (Torre) pode mover-se em qualquer direção ortogonal, enquanto que uma torre de 3 discos (Cavalo) move-se em forma de 'L' em cima de outra torre.

O jogo termina quando a primeira torre com seis ou mais discos (o Rei) é construída. O vencedor é o jogador que tem a sua cor no topo dessa torre.

Links de Referência:
- https://www.boardspace.net/sixmaking/english/Six-MaKING-rules-Eng-Ger-Fra-Ro-Hu.pdf


## Lógica do Jogo (285w por tópico)


### Representação Interna do Estado do Jogo
- codigo com as boards: listas de listas com x e o (falas dos 2 jogadores)

### Visualização do Estado do Jogo
- prints da board no sicstus
- maiusculas e minusclas, letras para cada tamanho de torre
- boards iniciais e as intermedias


### Validação e Execução de Jogadas


### Lista de Jogadas Válidas



### Fim do Jogo

No predicado do ciclo do jogo, é verificado se o jogo terminou, através do predicado game_over/2. Este predicado itera sobre todo o tabuleiro com o ``iterate_boad/2``, verificando se existe alguma torre com 6 ou mais discos, e em caso positivo, chamará  o predicado ``top_to_player/2``, para determinar o vencedor do jogo, analisando o disco presente no topo dessa torre. Caso contrário, o jogo continua no outro predicado do ``game_cycle/2``.

```prolog
game_cycle(_OldGameState, GameState):-
    [Board, _Player] = GameState,
    game_over(Board, Winner), !, 
    
    length(Board, Size),
    display_game(Size, Board),
    
    write('GAME OVER\n'), nl,

    show_winner(Winner).

game_over(Board, Winner):-
  iterate_board(Board, Top),
  top_to_player(Top, Winner).
```

### Avaliação do Estado do Jogo -> MELHORAR
Cada tabuleiro é avaliado tendo em conta a altura das torres presentes no tabuleiro atual e o topo dessas torres. 

Para cada jogador, são consideradas as alturas de todas as torres cujo topo lhe pertence. Para cada uma das alturas dessas torres, é atribuído um peso diferente, somando o seu quadrado ao valor total. Por exemplo, para uma torre de altura 2, é somado 2\*2=4, enquanto que para uma torre de tamanho 5, é somado 5\*5=25. Se a altura for maior do que 6, são somados 100 pontos de bónus ao seu valor total, com o objetivo de tornar evidente que o jogador ganhou. 

Os parâmetros altura e topo foram considerados devido à natureza do jogo e da caracterização do vencedor, uma vez que este será o primeiro a formar uma torre pelo menos 6 discos, com o topo seu.

O valor final no algoritmo de Minimax terá em conta a diferença entre os valores obtidos para cada jogador, evidenciando assim a vantagem de um jogador sobre o outro.

```
value(Board, Player, Value) :-
  iterate_board(Board, XValue, OValue),
  (Player == player1 -> Value is XValue - OValue; Value is OValue - XValue).
```


### Jogadas do Computador


## Conclusões -> DIMINUIR PALAVRAS (max 250w)
O jogo SixMaking foi implementado, com sucesso, em Prolog, apresentando 2 tamanhos de tabuleiro (4x4 e 5x5) e os 3 modos de jogo esperados: Player vs. Player, Player vs. Computer e Computer vs. Computer.

Simultaneamente, foi conseguida uma implementação do nível fácil e difícil do computador. Para este último nível, *greedy*, a implementação de um algoritmo Minimax mostrou ser bastante desafiante. Foi necessário delinear uma métrica de valorização dos tabuleiros, criar um algoritmo que tivesse em conta dois níveis de profundidade e a atribuição de “mínimo” e “máximo” ao valor, distinguindo o jogador. Nesta fase, uma possibilidade de aperfeiçoamento estaria nos níveis de profundidade a serem tidos em conta no algoritmo. Devido ao número elevadíssimo de possíveis jogadas em cada estado do jogo, a análise de apenas dois níveis mostrou ser uma opção eficiente e boa em diversos estados do jogo.

No modo de jogo entre dois computadores difícil vs. difícil, permitimos uma escolha aleatória entre jogadas igualmente valorizadas, fazendo com que o jogo não seja sempre o mesmo.

Todas as jogadas são também corretamente validadas e é impedido que a jogada escolhida pelo jogador atual desfaça a jogada anterior do oponente. O estado do jogo anterior é guardado e, desta forma, não ocorrrerá um ciclo infinito.

Durante todo o projeto, os conceitos lecionados nas aulas práticas e teóricas foram aplicados e consolidados ao longo dos vários desafios.


## Bibliografia

As regras e funcionamento do jogo foram consultadas nos seguintes links:
- https://www.boardspace.net/sixmaking/english/Six-MaKING-rules-Eng-Ger-Fra-Ro-Hu.pdf
- https://silp.iiita.ac.in/wp-content/uploads/PROLOG.pdf
- https://www.youtube.com/watch?v=FHdltzwaAJg
- https://sicstus.sics.se/documentation.html
