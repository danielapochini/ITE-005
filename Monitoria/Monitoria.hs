module Monitoria where 
import           Data.Char

data Pessoa = Pessoa {nome :: String, idade :: Int} deriving Show
                      

mudarParaMaiusculo :: [Pessoa] -> [Pessoa]
mudarParaMaiusculo [] = []
mudarParaMaiusculo ps = foldl pessoaMaiusculo [] ps

pessoaMaiusculo :: [Pessoa] -> Pessoa -> [Pessoa]
pessoaMaiusculo ps (Pessoa n i) = [(Pessoa (maiusculo n) i)] ++ ps  -- > head [1..5]

maiusculo :: String -> String
maiusculo s = map toUpper s

    -- map (\p -> head(pessoaMaiusculo [] p))   

-- Filtre numeros maiores que 7

filtro :: [Int] -> [Int]
filtro is = foldl (\a i -> if i > 7 then a
                    else i:a) [] is 

f :: [Int] -> Int -> [Int]
    f is i 
    | i > 7 = is
    | otherwise i : is