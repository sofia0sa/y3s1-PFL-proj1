The README file should be structured as follows:

● Identification of the topic (game) and group (group designation, student number and full name of each
member of the group), as well as an indication of the contribution (in percentages, adding up to 100%)
of each member of the group to the assignment;

● Installation and Execution: include all the necessary steps for the correct execution of the game in both
Linux and Windows environments (in addition to the installation of SICStus Prolog 4.8).

● Description of the game: a brief description of the game and its rules (up to 350 words); you should also
include the links used to gather information (official game website, rule book, etc.)

● Game Logic: 
Describe (merely copying the source code is not enough) the design and implementation
of the game logic in Prolog. The starting predicate must be play/0. This section should have information
on the following topics (up to 2000 words in total):
o Internal Game State Representation: describe how the game state is represented, including board
(typically using list of lists with different atoms for the pieces), current player, and possibly captured
and/or pieces yet to be played, or other information that may be required, depending on the game.
It should include examples of the Prolog representation of initial, intermediate and final game
states, and an indication of the meaning of each atom (i.e. how different pieces are represented).
o Game State Visualization: description of the game state display predicate implementation. It
should include information about the created menu system, as well as interaction with the user,
including input validation. The display predicate should be called display_game(+GameState),
receiving the current state of the game and the player who will make the next move. Appealing and
intuitive visualizations will be valued. Flexible game state representations and visualization
predicates will also be valued, for instance those that work with any board size, using an
initial_state(+Size, -GameState) predicate that receives the board size as an argument and returns
an initial game state.
o Move Validation and Execution: describe how a play is validated and executed, obtaining a new
game state. The predicate responsible for move validation and execution should be called
move(+GameState, +Move, -NewGameState).
o List of Valid Moves: describe how to obtain a list of possible moves. The predicate should be named
valid_moves(+GameState, +Player, -ListOfMoves).
o End of Game: verification of the end of the game, with identification of the winner. The predicate
should be called game_over(+GameState, -Winner).
o Game State Evaluation: describe how to evaluate the game state. The predicate should be called
value(+GameState, +Player, -Value).
o Computer Plays: describe how the computer chooses a move, depending on the level of difficulty.
The predicate should be called choose_move(+GameState, +Player, +Level, -Move). Level 1 should
return a valid random move. Level 2 should return the best play at the time (using a greedy
algorithm), considering the evaluation of the game state, as described above.
● Conclusions: Conclusions about the work carried out, including limitations of the program (known
issues), as well as possible improvements (roadmap) (up to 250 words);
● Bibliography: List of books, papers, web pages and other resources used during the development of
the assignment.
You can also include one or more imagesillustrating the execution of the game, showing initial, intermediate
and final game states (these game states can be hard-coded directly into the code file for this demonstration
of the game state visualization, using predicates similar to the initial_state/2 predicate).


# SixMaking
## Grupo: SixMaking_7
Guilherme Brandão Monteiro (up202108668)
Sofia Resende Ferreira de Sá (up202108676)

## Instalação e Execução

Para executar o jogo, é necessário instalar o **SICStus Prolog** (de preferência, a versão 4.8.0), fazer *download* dos ficheiros presentes em **PFL_TP1_T06_SixMaking_7.zip** e descompactá-los. Dentro do diretório **src**, deve consultar o ficheiro **game.pl**, através de [File] -> [Consult] -> [game.pl] ou diretamente da linha de comandos do SICStus Prolog. Por fim, para iniciar o jogo, basta executar o comando **``play.``**. O jogo está disponível em ambientes Windows e Linux.


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
