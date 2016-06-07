module Aula8 where
import Control.Monad 

-- instance Monad Maybe where
--      return = Just
--      (Just x) >>= f = f x
--      Nothing >>= f = Nothing


{-- 

P2: MONAD E APLICATIVO

Neste caso, a transformação natural abaixo 
pega o inteiro e o coloca sob a ação de um funtor Maybe

transfNat :: Int -> Maybe Int 
transfNat x = Just (x+1)

(transfNat.transfNat) 3 = 
(transfNat(3)) = transfNat(Just 4)
= Just(1 + Just 4) = Just(Just 5)

int e maybe int sao coisas distintas, int e [int] são coisas distintas

=================================
transfNat :: Int -> [Int] 
transfNat x = [x+1]
(transfNat.transfNat) 3 = 
(transfNat(3)) = transfNat([4])
= [1 + [4]]

=================================
fmap transfNat (transfNat(3)) =
fmap transfNat [4] =
[transfNat 4] = [[5]] | matriz de 5, ainda não é isso

solução = utilizar join
=================================
LEMBRETE: JOIN SEMPRE JUNTA DOIS FUNTORES EM UM

join (fmap transfNat (transfNat(3))) =
join (fmap transfNat [4]) =
join [transfNat 4] = join[[5]] = [5]

=================================

--}



transfNat :: Int -> [Int]
transfNat x = [x+1]

f :: Int -> Int
f x = x+1

{--
f(g(3)) = 6
----------
(f.g) 3 = f(g(3))
primeiro calcula g em 3 resultando em 5,
depois f em 5, resultando em 6

--}


tnf :: Int -> [Int]
tnf x = [x+1]

tng :: Int -> [Int]
tng x = [x+2]

g :: Int -> Int
g x = x+2

{--
join (fmap tnf (tng(3))) = [6]

(tn f <=< tng) 3 = join (fmap tnf (tng(3)))
primeiro calcula tng em 3 resultando [5],
depois joga tnf via fmap para dentro de [5], resultando em [tnf 5] = [[6]]
e finalmente o join elimina uma lista, resultando em [6]

setinha serve para compor duas transformações, exercicio anterior foi visto que
transformações com (.) não serve para funtores.
OBS: <=< é associativa

<=< chama-se KLEISLI AROW e vive dentro do Control.Monad

KLEISLI serve para composição de transformações naturais
e o BIND (>>=) serve para aplicação de transformações naturais
em um valor MONADICO.

O (>>=) tem a MESMA FUNÇÃO do ($), porém o ($) pode ser omitido,
já o BIND não.

=============================================
:t ($) = ($) :: (a -> b) -> a -> b
f $ 4 = 5
:t (>>=) :: (Monad m, Num (a -> m b)) => m a -> m b
[4] >>= tnf = [5] - valor monedico, uma função que é uma transformação natural


Just 4 >>= \x -> (Just (x+1) >>= \y -> Just (x+y)) =
\(x=4) -> (Just 5 >>= \y -> Just (x+y) = Just 9

=============================================
(f.g.f) 2 = 6
é igual a 
(tnf <=< tng <=< tnf) 2 = [6]

=============================================

Monad (São os funtores que compõem)
É uma monoide na categoria dos endofuntores
(Objetos : transformações naturais)

Se uma Monad é um Monoide
return seria o mempty
<=< seria o mappend

> Toda monad é um funtor mas nem todo funtor é uma monad <
lista, maybe, IO, = monad

=============================================
            Comparativo
                                m kind 2
f :: b -> c         tnf :: b -> m c
g :: a -> b         tng :: a -> m b
f.g :: a -> c       tnf <=< tng :: a -> m c
=============================================

EXERCICIOS:

=============================================

a) [7] >>= return (return seria como id)
[7]

b) Just 8 >>= \x -> Just (x-2) >>= \y -> Just (x*y)
\(x=8) -> Just 6 >>= \y -> Just (8*6) = Just 48

c) Just 4 >>= \x -> Nothing (sempre que ver Nothing = Nothing)
Nothing 

d) Nothing >>= \x -> Just (x+1)
Nothing

e) [4] >>= \_ -> [2]
[2]

OBS:
[4] >> [2] 
>> ignora o primeiro parametro
===========================================

Just 5 >>= \x
    Just (x+1) >>= \y
    Just (2+x) >>= \z
    Just (x+y+z)
    
x=5 y=6 z=7 Just 18

programação imperativa

f(){
    x = Just 5
    y = Just (x+1)
    z = Just (2+x)
    return (x+y+z)
}

=====================================

--} 

expr :: Maybe Int
expr = Just 5 >>= \x -> Just (x+1) >>= \y -> Just(2+x) >>= \z -> Just (x+y+z) 

expr' :: Maybe Int
expr' = do
      x <- Just 5
      y <- Just (x+1)
      z <- Just (2+x)
      return (x+y+z)
      
-- data () = ()
-- IO () seria o void do java
-- putStrLn é pura pois tem monad, seria o Syso mas é impuro 

-- as >> sempre vem depois de algo IO()

--forma funcional
main :: IO ()
main = putStrLn "Digite um nome: " >>
       getLine >>= \x -> putStrLn ("Ola " ++ x)
       
      
main2' :: IO ()
main2' = putStrLn "Digite um numero: " >>
         readLn >>= \x -> putStrLn ("O numero eh: " ++ show(x+1))       


-- forma estruturada
main' :: IO ()
main' = do
        putStrLn "Digite um nome: "
        x <- getLine
        putStrLn ("Ola " ++ x)
        
        
main2 :: IO ()
main2 = do
      putStrLn "Digite um numero: "
      x <- readLn
      putStrLn ("O numero eh: " ++ show(x+1))
      
      
-- na notacao Do nao há uso de operadores monadicos
-- >>, >>=, =<<, ...      

{--
EXERCICIOS:

1) Faça um programa que receba uma String do Teclado e mostre-o em ordem reversa
(estruturado)

2) Faça um programa que multiplique dois numeros (funcional)

--}
   
aoContrario :: IO ()
aoContrario = do
        putStrLn "Digite uma palavra "
        x <- getLine
        putStrLn (reverse x)

mult :: IO ()
mult = putStrLn "Digite o primeiro numero: " >>
         readLn >>= \x -> putStrLn "Digite o segundo numero: " >> 
         readLn >>= \y -> putStrLn ("A multiplicação é: " ++ show(x*y))      