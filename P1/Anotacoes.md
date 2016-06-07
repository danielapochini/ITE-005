Linguagem preguiçosa (Laziness)
Tipagem Forte
-> Se compila é pq está certo
Linguagem Pura
Não existem coisas globais dentro do código
 

Irc - #haskell canal freenode

Extensão .hs obrigatória
Ctrl+s sempre salva, nome do arquivo = nome do modulo

C9.io: 
sudo apt-get install haskell-platform
ghci -> chamar o compilador
:l Aula1(nomedoarq) -> compila
:r reload - compila

somarUm (dobro8) == somarUm . dobro $8  resultado: 17
chama primeiro o dobro depois o somarUm , $ tira o parenteses


===============================================================

Exemplos Aula 3

Exemplos pattern matching p/ listas

3:[1,4] -> [3,1,4]
x: xs
[1] -> x=1 xs=  []
[6,8,3,7]

x:y: xs
x=6
y=8
xs=3,7
"FATEC"
x:xs
x= 'F' ::char
xs = "ATEC"

