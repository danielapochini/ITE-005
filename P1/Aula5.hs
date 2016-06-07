module Aula5 where 

-- Integer tem uma precisão melhor
fat :: Integer -> Integer
-- 0 é um value constructor de Integer e não uma condição booleana
fat 0 = 1
fat k = k * fat (k-1)

--  fat 5 = terminal 120
--  map fat [1,2,3,4,5] = terminal [1,2,6,24,120]
-- lembrando que map joga a função pra todo mundo da lista


{--
TESTE DE MESA                           MEMORIA 
fat fat 0 = 1                               
fat k = k* fat (k-1)
                                         
fat 5 = 5 * fat 4 = 5*24 = 120           fat 0 <- sai fora com valor 1
fat 4 = 4 * fat 3 = 4*6 = 24             fat 1 <- fora
fat 3 = 3 * fat 2 = 3*2 = 6              fat 2 <- fora
fat 2 = 2 * fat 1 = 2                    fat 3 <- fora
fat 1 = 1 * fat 0 = 1*1 = 1              fat 4 <- fora
fat 0 = 1                                fat 5 <- fora


--}

-- GUARDS: pra cada pipe | inserido ele vai testar a condicao ate chegar no otherwise
-- Série de verificação booleana executadas em ordem até chegar em otherwise
-- quando chega no otherwise é pq tudo antes falhou

fat' :: Integer -> Integer
fat' k 
    | k <= 1 = 1
    | otherwise = k * fat (k-1)
    
fib :: Integer -> Integer
fib n
    | n <= 2 = 1
    | otherwise = fib (n-1) + fib (n-2)
--  map fib [1 .. 10] = terminal [1,1,2,3,5,8,13,21,34,55]

eliminarVogais :: String -> String
eliminarVogais [] = [] -- concatenar o vazio, vazio retorna vazio, condição de parada
eliminarVogais (x:xs) 
                | elem x "AEIOUaeiou" = eliminarVogais xs -- perguntando se x é uma vogal
                | otherwise = x : eliminarVogais xs --  ':' colocar na frente de uma lista // perguntando se é consoante
                
{--
                        TESTE DE MESA         ||            MEMORIA
eV = eliminarVogais
                                    
                                    
                                    
eV "FATEC" = 'F' : eV "ATEC" = 'F' : 'T' : 'C' : []        eV = [] <- 1º fora
eV "ATEC" = eV "TEC" = 'T' : 'C': [] = "TC"                eV = "C"
eV "TEC" = 'T' : eV "EC" = 'T' : 'C' : [] = "TC"           eV = "EC"
ev "EC" = eV "C" = 'C' : [] = "C"                          eV = "TEC"
ev "C" = 'C' : eV [] = 'C' : [] = "C"                      ev = "ATEC"   
eV [] = []                                                 ev "FATEC"

--}



{--
EXERCICIOS:

1) Faça recursivamente o que se pede:

a) Elimine todas as palavras de 4 letras de uma lista de Strings e faça o teste de mesa.

b) Elimine todos os pares 'AA' de uma String e faça o teste de mesa.

--}

e4 :: [String] -> [String]
e4 [] = [] 
e4 (x:xs)
    | length x == 4 = e4 xs
    | otherwise = x : e4 xs
--  e4 ["FATEC", "SI", "NAO", "TIRA", "PHP", "DA", "CABECA"] RESULTADO = ["FATEC","SI","NAO","PHP","DA","CABECA"] 
-- tira unica de 4 letras e saiu fora


eA :: String -> String 
eA [] = []
eA (x:y:xs) 
    | elem x "A" && elem y "A" = eA xs
    | otherwise = x : eA (y : xs)
eA (x:xs) = x:xs

{-- Teste de Mesa
x = 'T'
y = 'A'
xs = 'AAB'

eA "TAAB" = 'T' : eA "AAB" = "TB"
eA "AAB" = eA "B" = "B"
x = 'a' y = 'a' xs = 'b'
eA "B" = "B"
x = "B" xs = []

--}

foo :: String -> String
foo [] = []
foo (x:y:z:xs)
    | x == 'A' && y == 'B' && z == 'C' = foo xs
    | otherwise = x : foo (y:z:xs)
foo x = x


{--
x = 'D'
y = 'A'
z = 'B'
xs = 'CD'

foo "DABCD" = 'D' : foo 'ABCD' = "DD"
foo "ABCD" = foo 'D'
foo 'D' = 'D' 

x= D xs = []


--}

 

{-- 
Foldable t => (b -> a -> b) -> b -> t a -> b

soma = b
função = (b->a->b)
v[] = t a

int soma = 0 (b)
int [] v = [1,2,3,5,10]
for (int i=0; i< v.length; i++){
    soma = soma + v[i];
}
return soma;

--}


{-- TESTE DE MESA
foldl (+) 0 [1,2,3,5,10] = 21

= foldl (+) (0+1) [2,3,5,10]
= foldl (+) (0+1+2) [3,5,10]
= foldl (+) (0+1+2+3) [5,10]
= foldl (+) (0+1+2+3+5) [10]
= foldl (+) (0+1+2+3+5+10) []
= (0+1+2+3+5+10) = 21


foldl (*) 1 [1,2,3,5,10] = 300
--}


{-- 
foldl (\x y -> EXPR ENVOLVENDO X E/OU Y) valor inicial [lista]
x representa a acumulação do valor inicial com o resultado da expressão
y é alguem da lista
sempre pode ignorar se necessario, x ou y do lambda
-> (\_ y -> EXPR) y sempre da lista// (\x _ -> EXPR) x sempre da acumulação // (\x y -> EXPR)
o FOLDL sempre usa uma função de dois parametros


=========
f = foldl (\x y -> x + length y)
                             x    y   
foldl (\x y -> x + length y) 0 ["FATEC", "SANTOS", "AMA", "JAVA"] = 18

primeira vez = 0 + 5 [...] e assim por diante

foldl f (0+ length "FATEC") ["SANTOS", "AMA", "JAVA"] 
foldl f (0+ length "FATEC" + length "SANTOS") ["AMA, "JAVA"]

--}

{-- EXERCICIOS II

a) faça um FOLDL para contar letras de uma palavra
b) faça um FOLDL para reverter uma string

--}


c = foldl (\x _ -> x+1) 0 "FATEC SANTOS"

--                    string por causa do retorno
r = foldl (\x y -> [y] : x) [] "DANIELA"
--                    String