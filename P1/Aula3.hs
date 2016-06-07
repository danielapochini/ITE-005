module Aula3 where 

-- MONITORIA = TERÇA 18H / SEXTA 18H

{-
Toda string é um [CHAR], por isso que usamos [x], pois x é um char 
-}

teste0:: String -> String
teste0 (x:xs) = xs ++ [x] 
-- teste0 "FATEC" = "ATECF"

teste (x:y:xs) = [y] ++ xs ++ [x] 
-- teste "FATEC" = "ATECF"

teste1 (x:y:xs) = reverse xs ++ [y]
--"CETA"

teste2 (x:_:xs) = reverse xs ++ [x]
-- "CETF"


-- x:y:[] indica apenas duas letras
teste3 (x:y:[]) = [y] ++ [x]
teste3 _ = "ERRO..."
-- ERRO... pois nao termina 

--xs pelo menos. [] igual

-- x:y:z:xs pelo menos 3 letras
teste4 (x:y:z:xs) = [z] ++ [x] ++ xs
-- TFEC

{- EXERCICIO
Crie uma função que troque a segunda pela quarta letra e a primeira pela terceira
--} 

exercicio (x:y:z:w:xs) = [z] ++ [w] ++ [x] ++ [y] ++ xs 
-- D1 A2 N3 I4 E5 L6 A7 = DANIELA = "NIDAELA"

-- value constructors com 2 campos - Fisica, value constructor Juridica, 1 campo
data Pessoa = Fisica String Int|
                Juridica String deriving Show
--Fisica "Daniela" 22

-- :t Fisica
-- Fisica :: String -> Int -> Pessoa
-- t: Juridica
-- Juridica :: String -> Pessoa 

--pro haskell todo value constructor é uma função

-- Fisica nome idade é um Pattern matching que encaixa no tipo declarado
-- A variavel nome é uma string e a variavel idade é um Int
aniversario :: Pessoa -> Pessoa 
aniversario (Fisica nome idade) = Fisica nome (idade+1)
-- aniversario (Fisica "Daniela" 22) = Fisica "Daniela" 23
-- aniversario . aniversario . aniversario $Fisica "Daniela" 22 = Fisica "Daniela" 25
aniversario (Juridica nome) = Juridica nome

{- SEGUNDO EXERCICIO
Faça a função fusão que concatene os campos nome e some os campos idade.
A função deve funcionar para os dois value constructors.
-}

-- + soma / ++ concatena 

fusao :: Pessoa -> Pessoa -> Pessoa 
fusao (Fisica nome idade) (Fisica nome1 idade1) =  Fisica (nome++nome1) (idade + idade1)
fusao (Juridica nome) (Juridica nome1) = Juridica (nome ++ nome1)


-- :t personName = personName :: Person -> String (como se fosse o Get)
-- recebe Pessoa devolve uma String
-- :t personAge = personAge :: Person -> Int
-- recebe Pessoa devolve um Int 

-- Sintaxe de Registro = Record Syntax
-- algo que ajuda a dar o nome aos campos dos value constructors 
data Person = Fis {personName :: String, personAge :: Int} |
                        Jur {personName :: String} deriving Show

{- Fis "Daniela" 22 = Fis {personName = "Daniela", personAge = 22}
 Jur "Fatec" = Jur {personName = "Fatec"} -}
 
-- personAge (Fis "Daniela" 22) = extrai a idade e devolve 22

somarIdades :: Person -> Person -> Int 
-- recebe duas pessoas e devolve um Int usando pattern matching
somarIdades (Fis _ i) (Fis _ i1) = i+i1
somarIdades _ _ = 0
-- somarIdades (Fis "Daniela" 22) (Fis "Giulia" 23) = 45

somarIdades' :: Person -> Person -> Int 
somarIdades' x y = personAge x + personAge y
-- somarIdades' (Fis "Daniela" 22) (Fis "Giulia" 23) = 45

-- Problema desse acima: se passar Pessoa Juridica ele se perde, 
-- quando tem o pattern matching ele evita os exceptions

data Sozinho = Sozinho {a :: String, b :: Char} deriving Show
-- é possivel criar um value constructor com o mesmo nome do data constructor

{- TERCEIRO EXERCICIO:
Faça o tipo Ponto com dois campos Double representando as posições x e y na tela.
Faça as funcoes: 
moverX: move um Ponto dx unidades na direção x.
A variavel dx é um parametro 
moverY: move um Ponto dy unidades na direção y.
A variavel dy é um parametro
mag: Extrai a distancia da origem de um ponto
-}

data Ponto = Ponto {x :: Double, y :: Double} deriving Show

moverX :: Ponto -> Double -> Ponto 
moverX (Ponto x y) dx = Ponto (x+dx) (y)

moverY :: Ponto -> Double -> Ponto
moverY (Ponto x y) dy = Ponto (x) (y+dy)

mag :: Ponto -> Double
mag (Ponto x y) =  square (2^ (x + y))

