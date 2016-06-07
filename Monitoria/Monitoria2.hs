module Monitoria2 where 

-- fazendo uma estrutura ficar maior

aumentar :: [Int] -> [Int] 
aumentar [] = []
aumentar xs = foldl (\a b -> a ++ a ++ [b]) [] xs 

-- Arvore

data Arvore = Galho Int (Arvore) (Arvore) 
            | Folha Int | Nada deriving Show


-- ERD ESQUERDA RAIZ DIREITA
emOrdem :: Arvore -> [Int] 
emOrdem Nada = [] 
emOrdem (Folha i) = [i] 
emOrdem (Galho i gEsq gDir) = emOrdem gEsq ++ [i] ++ emOrdem gDir -- concatenando lista

-- EDR
posOrdem :: Arvore -> [Int] 
posOrdem Nada = [] 
posOrdem (Folha i) = [i] 
posOrdem (Galho i gEsq gDir) == (f gEsq) ++ (f gDir) ++ [i]

-- RED
preOrdem :: Arvore -> [Int] 
preOrdem Nada = [] 
preOrdem (Folha i) = [i] 
preOrdem (Galho i gEsq gDir) == [i] ++ (f gEsq) ++ (f gDir)


-- Testes
-- emOrdem Nada = []
-- emOrdem (Folha 5) = [5]
-- emOrdem (Galho 5 Nada Nada) = [5]
-- emOrdem (Galho 5 (Galho 2 (Folha 1)(Folha 3)) (Galho 7 (Folha 6) Nada)) = [1,2,3,5,6,7]


-- ericshortcut@gmail.com