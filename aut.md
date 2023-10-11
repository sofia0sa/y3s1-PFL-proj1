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