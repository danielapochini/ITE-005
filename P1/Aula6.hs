module Aula6 where
import Data.Monoid

-- data Tipo = ValueC1 Char | ValueC2 Char Char deriving Show
-- ValueC1 x
-- ValueC2 y w

--kind 1, int, CHAR, bool 
--kind 2 String, lista, Bolsa, Guarda Roupa (camisa,cobertor... guarda roupa de Itens)

-- a é chamado de Type Parameter 
data Bolsa a = Bolso a | Necessaire a a | Vazio  deriving Show
-- (Eq a) => restringe aos tipos A que sejam comparaveis 
instance (Eq a) => Eq (Bolsa a) where 
    (Bolso x) == (Bolso y) = x == y 
    (Necessaire a b) == (Necessaire c d) = ((a == c) && (b == d)) || ((a == d) && (b == c))
    _ == _ = False
    
-- MONOIDE

instance (Monoid a) => Monoid (Bolsa a) where
    mempty = Vazio
    mappend x Vazio = x
    mappend Vazio x = x
    mappend (Bolso a) (Bolso b) = Bolso (a <> b)
    mappend (Bolso a) (Necessaire b c) = Bolso (a <> b <> c)
    mappend (Necessaire b c) (Bolso a) = Bolso (a <> b <> c)
    mappend (Necessaire a b) (Necessaire c d) = Necessaire (a <> c) (b <> d)

{-- instance (Show a) => Show (Bolsa a) where 
    show (Bolso x) = "Uma linda bolsa: " ++ show x
    show (Necessaire x y) = "Uma linda bolsa: " ++ show x ++ " - " ++ show y
    --} 

-- Typeclass
class (Numero a ) where
    numero :: a -> Int

-- Instance de numero para Item

--instance Sim Item where
--numero Batom = 1



{--
MONOIDE (Data.Monoid)

Dado um conjunto M != Ø e uma operação binária <>.
Uma monoide (M, <>) é uma etrutura que satisfaz:

(associatividade)(∀a, b, c ∈ M) (a <> b) <> c = a <> (b <> c) 
(neutro-mempty) (∃e ∈ M)(∀a ∈ M) m <> m = m 
(<>) = mappend

M = Z
mempty = 0
<> = +
----------
M = Z
mempty = 1
<> = *
----------
M = [a]
mempty = []
<> = ++


--} 



-- deriving Eq = itens são comparaveis 
data Item = Batom | Espelho | Celular | Chave deriving (Eq,Show)
-- Eq é um type class, logo para cada tipo do Haskell vc deve criar uma instancia
-- para definir o comportampeno desejado

{--
Faça uma função Trocar, que troca as posições (contendo Item) de dentro 
da necessaire para o Bolso, não acontece nada. --} 

-- como bolsa possui kind 2 *->*
-- vc deve especificar o tipo de dentro
--(nem que seja a, que representa qualquer tipo) SEMPRE

trocar :: Bolsa Item -> Bolsa Item   
trocar (Necessaire x w) = (Necessaire w x) 
--trocar (Necessaire Batom Espelho) = Necessaire Espelho Batom
trocar x = x 
-- trocar (Bolso Batom) = Bolso Batom