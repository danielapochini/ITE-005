module Aula7 where

{--
OBJETO, POLIMORFISMO, COMO COMPOR, IDENTIDADE, ASSOCIATIVIDADE, ELEMENTO NEUTRO

ob(6) = { A, P, M} conjuntos
A = {Vanessa, Mauro, André, Matheus}
P = {Garcia, Chiara, Ciro}
M = {ITE-005, IDS-002, ISW-012}

eOrientando :: A -> P
gostar :: A -> M
atribuido :: M -> P

eOrientando(Mauro) = Garcia
gostar(Vanessa) = IDS-002
atribuido(IDS-002) = Ciro
atribuido.gostar(Vanessa) = Ciro
atribuido.gostar :: A -> P

idM.f (Vanessa) = idM (f(Vanessa)) = idM(IDS-002) = IDS-002

f.idA(Vanessa) = f(idA(Vanessa)) = f(Vanessa) = IDS-002

==========================
associatividade: f.(g.h) = (f.g).h

f:: A -> B       idA :: A -> A
g:: B -> A       idB :: B -> B
h:: A -> B

f.idB = f
idA.h = h 
f !=h , nao é categoria

========================
f :: Int -> Int
f x = x + 1 

g :: Int -> Int
g x = 2 * x
(f . g)5 = f(g(5)) = f(10) = 11

====================
identidade

id :: a -> a
id x = x
id é ele mesmo

===================

 
--}

data Humano = Bia | Pedro deriving Show

data Vampiro a = Vampiro a deriving Show
-- vampiro kind 2, pra ser funtor tem que ser kind 2.
instance Functor Vampiro where
    fmap f (Vampiro x) = Vampiro (f x)
    --fmap faz pular o "rotulo"

amor :: Humano -> Humano
amor Bia = Pedro 
amor Pedro = Bia
--  fmap amor (Vampiro Bia) = Vampiro Pedro

data Valido a = Sim a | Nao 
-- data Maybe a = Just a | Nothing (funcao do haskell)

instance Functor Valido where
    fmap f (Sim x) = Sim (f x)
    fmap f Nao = Nao

--fmap (2*) (Sim 8) = 16 // fmap (2*) Nao = Erro...

instance (Show a) => Show (Valido a) where
    show (Sim x) = show x
    show (Nao) = "Erro..."

safeDiv :: Double -> Double -> Valido Double
-- pode retornar um valor valido ou nao
safeDiv x 0 = Nao
-- safeDiv 19 0 = Nao
safeDiv x y = Sim (x/y)
--  safeDiv 4 2 = Sim 2.0 
--Diferença de 2 p/ Sim 2.0 = passou por uma função que a validou


--funcao infixa, vira um operador apenas colocar função em parenteses
(/?) :: Double -> Double -> Maybe Double
(/?) x 0 = Nothing
(/?) x y = Just (x/y)

-- 8 /? 4 = Just 2.0
-- 7 /? 2 = Just 3.5
-- 10 /? 0 = Nothing 



-- Transformações naturais
toJust :: a -> Maybe a 
toJust x = Just x
-- converte pra Just, mudando o rotulo

toList :: a -> [a]
toList x = [x]

toList' :: Int -> [Int]
toList' x = [x+1]
{--
toList' (toList' 3)
toList' [4] = [4] + 1 = errado pois nao se compoem

--}

f :: Int -> Int 
f x = x+1
