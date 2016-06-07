module Aula1 where 

-- PARA FAZER UMA FUNÇÃO HASKELL
-- nome da função e parametro 
-- nome x1 x2 x3 x4 ... xn (nao separa por virgula) = expressão

-- dobro HAS TYPE > TEM O TIPO < no parametro e na saida
-- dobro :: Int é errado pois tem duas coisas para tipar
dobro :: Int -> Int

-- primeiro Int tipo do x, segundo Int da expressão, do retorno da função
-- todo mundo antes tipo do parametro, depois tipo da função
dobro x = 2*x

somarUm :: Int -> Int 
somarUm a = a + 1

somar :: Int -> Int -> Int
-- 2 dos parametros, 1 da saida. 
--ultimo tipo é sempre da saida.
somar x y = x + y 


u:: Int 
u = 8
--u+u = 16

v:: [Int]
v = [1 .. 10]
-- [1,2,3,4,5,6,7,8,9,10]


-- sum v , soma o vetor = 55
-- sum v ++ v = 110
-- concatenar : v ++ a

-- let a = [1 .. 6 ] testes no terminal

a :: [Int]
a = [ 1 .. 6] 
-- [1,2,3,4,5,6]

b :: [Int]
b = [6, 5 .. 1]
-- [6,5,4,3,2,1]

w :: [Char]
w = ['A' .. 'Z']
-- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

y :: String
y = "Santos"
-- reverse y = SOTNAS 

-- LIST COMPREENSHIONS
-- virgula é o filtro, condição

-- [ EXPRESSAO | LISTA DE ITERAÇÃO , FILTRO, FILTRO, ..]

todosPares :: Int -> [Int]
todosPares n = [x | x<- [0 .. n] , mod x 2 == 0] 
-- [0,2,4]

multiplosCinco n = [ x | x <- [0 .. n], mod x 5 == 0]
-- [0,5,10,15,20,25,30]

-- x pode ser qualquer expressao 
-- todosMaisUm n = [(-1)*x | x <- [0 .. n]]
todosMaisUm n = [x+1 | x <- [0 .. n]]
-- [1,2,3,4,5,6,7,8,9,10,11]

wq n = [(-1)*x | x <- [0 .. n], x>3 , mod x 2 ==0] -- nao ha ordem pra exec 

-- O DOBRO DE TODOS OS NUMEROS DE 0 A N, MAIORES QUE 12 E MENORES QUE 142

exercicio :: Int -> [Int]
exercicio n = [x * 2 | x <- [0 .. n], x>12, x<142]