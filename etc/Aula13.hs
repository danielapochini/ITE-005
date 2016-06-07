module Aula13 where

import Control.Applicative 

-- um data constructor com value constructor de mesmo nome, 
-- dois campos usando record syntax

data Produtoz = Produtoz {produtozNome :: String,
                          produtozValor :: Double
                          } deriving Show

-- EX. 1 (COBRAR MEIO PONTO NA PROVA)
-- let f = Just (\x -> x + 1)
-- f <*> Just 5 = Just 6

{--
f com funtor e x com funtor = <*>
f sem funtor e x com funtor = <$> 
f sem funtor e x sem funtor = $ ou nada
--}

-- let soma = \x y -> x+y = soma 5 3 = 8
-- let f = Just soma (soma ficou dentro de um funtor)
-- f <*> Just 5 <*> Just 3 = Just 8
-- quanto mais parametro na função colocar <*>

{-- 

soma <$> Just 5 <*> Just 3
usa-se <$> pois soma está sem funtor

 f <*> Just 5 <*> Just 3,
usa-se <*> pois f possui funtor

===================================
(soma <$> Just 5) <*> Just 3
soma <$> Just 5 = (<$> Just 5 entra no LAMBDA)
(\x y -> x + y) <$> Just 5 =
Just ((\x y -> x + y) 5) =
Just (\y -> 5 + y)

==================================

Produtoz <$> (Just "Algo valido") <*> (Just 1200)
Just (Produtoz {produtozNome = "Algo valido", produtozValor = 1200.0})
Produto estava fora do Just, então o Produto todo foi para dentro do Just

--}