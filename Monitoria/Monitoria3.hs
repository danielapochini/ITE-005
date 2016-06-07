module Monitoria3 where

data Arvore = Galho Int Arvore Arvore | Folha Int | Nada deriving Show

mapF:: (Int -> Int) -> Arvore -> Arvore
mapF f Nada = Nada
mapF f (Folha x) = (Folha(f x))
mapF f (Galho x gEsq gDir) = (Galho (f x)(mapF f gEsq)(mapF f gDir))

arvore = (Galho 4 (Galho 2 (Folha 1) (Folha 3)) (Galho 6 (Folha 5) (Nada)))
f1 = mapF (*2) arvore
f2 = mapF (+1) arvore
f3 = mapF (\x -> foldl (\a x -> x*2) x [1..x])
f4 = mapF (\x -> foldl (\a xi -> a +(xi*1)) x ([x'|x' <- [1 .. x], ((mod x' 2) == 0)])) arvore 