module Aula4 where 

{- Os value constructors sao os valores dos data constructors
os value construtors podem carregam campos
-} 


-- () -> Unidade (Unity)

teste :: () -> Int
teste x = 0

teste2 :: Int -> ()
teste2 x = ()

data Sozinho

teste' :: Sozinho -> Int 
teste' x = 0

{- no haskell todo tipo tem seu undefined, inclusive os tipos
sem value constructor -} 

teste3 :: Int -> Sozinho
teste3 x = undefined

foo :: () -> Sozinho -> Int 
foo x y = 0

foo' :: [Sozinho] -> Int 
foo' x = 0
-- foo' [undefined] ou foo' [undefined, undefined, undefined]


{-
CURRYING: tecnica que consiste em transformar uma chamada de
função em uma sequencia de funções com menos parametros que
a original.
-}

somar :: Int -> Int -> Int 
somar x y = x + y

volume :: Double -> Double -> Double -> Double
volume x y z = x*y*z

{-
 :t volume
volume :: Double -> Double -> Double -> Double
:t (volume 2)
(volume 2) :: Double -> Double -> Double
[...]
:t volume(1,1,1)
 volume(1,1,1) :: Double
 
=============
volume 1 2 3 

ou

          f  g  volume       
(((volume 1) 2) 3)

-}

{- 

LAMBDAS
Função anonima (sem corpo)
\p1 p2 p3 ... pn -> EXPR

let dobro = \x -> 2*x
dobro 4 = calcula a expressao lambda, resulta 8

(\x y z -> x*y*z) 1 2 3  (resultado 6) 
  1 2 3
    
-}

{-
LAMBDA + CURRYING

let somaaa = (\x y -> x + y) 3

= (\y -> 3+y) , 3 vai pro x, aguarda o y

-}

{-
HIGH-ORDER FUNCTION (FUNÇÃO DE ALTA ORDEM)
é uma função que recebe via parametro(s) funções e/ou
retorna funções.
permite que o haskell veja as funções como valores comuns
-}


aplicar :: (Int -> Int) -> Int 
-- aplicar (\x -> 2*x) = 13
aplicar f = 1 + f 6
-- Chama a função f com argumento 6 e soma 1

-- aplicar ((\x y -> x y) 10)
-- = primeiro executa dentro dos parenteses
-- aplicar (\y -> 10 +y)
-- = 1+ (\y -> 10 +y) 6
-- = 1 + 16
-- = 17

-- recebe inteiro devolve uma função
mult :: Int -> (Int -> Int) 
mult x = (\y -> x*y)

mult' :: Int -> Int -> Int
mult' x y = x*y

-- MAP
--map :: (a -> b) -> [a] -> [b]
--map (\x -> 2*x) [1,2,3,4,5]

--map (2*) [1..5]
--Retorna uma lista com o dobro da primeira (de 1 a 5)
-- [2*1, 2*2, 2*3, 2*4, 2*5]
-- [2, 4, 6, 8, 10]


-- FILTER
--filter :: (a -> Bool) -> [a] -> [a]
--filter (\x -> length x == 5) ["ABCDE", "FATEC", "SANTOS", "ABC"]
-- Retorna somente os que tem o tamanho igual a 5
-- ["ABCDE", "FATEC"]

--filter (>=4) [1..10]
--Retorna os maiores ou iguais a 4

--filter (\x -> elem x "AEIOU") "FATEC"
-- Retorna somente as vogais

--filter (\x -> notElem x "AEIOU") "FATEC"
-- Retorna somente as consoantes


data Pessoa = Pessoa String Int -- Um value constructor com dois campos
              deriving Show

maior :: Pessoa -> Bool
maior (Pessoa _ idade) = idade >=18
              
-- filter (\(Pessoa _ idade) -> idade>=18) --Versao Lambda da funcao maior               
filtrarMaiores :: [Pessoa] -> [Pessoa]
filtrarMaiores xs = filter maior xs
-- Executa o filter com a funcao maior no xs (vetor de Pessoas)

{-
Exercicios:

a) Faca o tipo Metragem com os value constructors Metro e Kilometro
b) Faca o tipo Medida que possua os campos Double e Metragem
c) Faca a funcao converter que converte Metros para Kilometros e vice-versa
d) Faca a funcao converterTodos que converte todos os elementos de uma lista de Medida
e) Faca uma funcao maior5 que filtra qualquer medida maior que 5 sem importar a Metragem

-}

-- a)
data Metragem = Metro | Kilometro
                deriving Show
                
--b)                
data Medida = Medida Double Metragem
              deriving Show
              
--c)              
converter :: Medida -> Medida
converter (Medida x Metro) = Medida (x/1000) Kilometro
converter (Medida x Kilometro) = Medida (x*1000) Metro
--Pattern Matching: Medida, x eh o Double, e Kilometro ou Metro eh a Metragem

--d)
converterTodos :: [Medida] -> [Medida]
converterTodos x = map converter x
-- let m = [Medida 2 Metro, Medida 10 Kilometro, Medida 1 Kilometro, Medida 1 Metro]
-- converterTodos m


--e)

maior' :: Medida -> Bool
maior' (Medida valor _) = valor > 5

maior5 :: [Medida] -> [Medida]
maior5 xs = filter maior' xs