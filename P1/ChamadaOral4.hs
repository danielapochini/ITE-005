module ChamadaOral4 where

{-- Exercício 1.7 
Sabe-se que as unidades imperiais de comprimento podem ser Inch, Yard ou Foot. 
Sabe-se que 1 inch (polegada) = 0.0254m, 1 yard (jarda) = 0.9144m, 1 foot (pés) = 0.3048m. --}

 
data Unidade = Polegada | Jarda | Pes | Metro deriving Show 
data Medida = Medida {valor :: Double, uni :: Unidade } deriving Show -- Sintaxe de Registro      

{-- 1) Faça a função converterMetros que recebe a unidade imperial 
e o valor correspondente nesta unidade e retorna o valor em metros. --}

converterMetros :: Medida -> Medida 
converterMetros (Medida valor Polegada) = (Medida (valor/39.370) Metro) --Polegada p/ Metros
converterMetros (Medida valor Jarda) = (Medida (valor/1.0936) Metro) 
converterMetros (Medida valor Pes) = (Medida (valor/3.2808) Metro) 
converterMetros met = met --Retorna ele mesmo

{-- 2) Faça a função converterImperial que recebe um valor em metros 
e a unidade de conversão e retorna o valor convertido para a unidade desejada --}

converterImperial :: Medida -> Medida
converterImperial (Medida valor Polegada) = (Medida (valor * 39.370) Polegada) -- Metros p/ Polegada
converterImperial (Medida valor Jarda) = (Medida (valor * 1.0936) Jarda) 
converterImperial (Medida valor Pes) = (Medida (valor * 3.2808) Pes) 
converterImperial imp = imp  


{--
Exercício 4.5 Implemente os percursos pós-ordem e pré-ordem. Via comentário, 
faça os ”testes de mesa” para os dois percursos usando as árvores:

• Da Figura 4.1.
Galho 50 (Galho 30 (Folha 20) (Folha 40)) (Galho 90 Nulo (Folha 100))

• Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula)) (Raiz 20 Nula (Raiz 22 (Folha 21) Nula))
Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) (Galho 20 Nada (Galho 22 (Folha 21) Nada))

--} 

data Arvore = Galho Int (Arvore) (Arvore) | Folha Int | Nada deriving Show

--ERD (Esquerda Raiz Direita)
emOrdem :: Arvore -> [Int]
emOrdem Nada = []
emOrdem (Folha x)=[x]
emOrdem (Galho x gEsq gDir)=emOrdem gEsq ++ [x] ++ emOrdem gDir


--EDR (Esquerda Direita Raiz)
preOrdem :: Arvore -> [Int]
preOrdem Nada = []
preOrdem (Folha x)=[x]
preOrdem (Galho x gEsq gDir)=preOrdem gEsq ++ preOrdem gDir ++ [x]


--RED (Raiz Esquerda Direita)
posOrdem :: Arvore -> [Int]
posOrdem Nada = []
posOrdem (Folha x)=[x]
posOrdem (Galho x gEsq gDir)=[x] ++ posOrdem gEsq ++ posOrdem gDir

{-

--ERD (Esquerda Raiz Direita)
emOrdem :: Arvore -> [Int]
emOrdem Nada = []
emOrdem (Folha x)=[x]
emOrdem (Galho x gEsq gDir)=emOrdem gEsq ++ [x] ++ emOrdem gDir

TESTE DE MESA:
emOrdem (Galho 50 (Galho 30 (Folha 20) (Folha 40)) (Galho 90 (Nada) (Folha 100)))
emOrdem (Galho 30 (Folha 20) (Folha 40)) ++ [50] ++ emOrdem (Galho 90 (Nada) (Folha 100))
emOrdem (Folha 20) ++ [30] ++ emOrdem (Folha 40) ++ [50] ++ emOrdem (Nada) ++ [90] ++ emOrdem (Folha 100)
[20] ++ [30] ++ [40] ++ [50] ++ [] ++ [90] ++ [100]
[20,30,40,50,90,100]

emOrdem (Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) (Galho 20 Nada (Galho 22 (Folha 21) Nada)))
emOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) ++ [15] ++ emOrdem (Galho 20 Nada (Galho 22 (Folha 21) Nada))
emOrdem (Folha 6) ++ [11] ++ emOrdem (Galho 12 (Folha 10) Nada)) ++ [15] ++ emOrdem Nada ++ [20] ++ emOrdem (Galho 22 (Folha 21) Nada))
[6]++[11]++emOrdem (Folha 10)++[12]++emOrdem Nada)++[15]++[]++[20]++emOrdem (Folha 21)++[22]++emOrdem Nada)
[6]++[11]++[10]++[12]++[]++[15]++[]++[20]++[21]++[22]++[]
[6,11,10,12,15,20,21,22]


--RED (Raiz Esquerda Direita)
posOrdem :: Arvore -> [Int]
posOrdem Nada = []
posOrdem (Folha x)=[x]
posOrdem (Galho x gEsq gDir)=[x] ++ posOrdem gEsq ++ posOrdem gDir

TESTE DE MESA:
posOrdem (Galho 50 (Galho 30 (Folha 20) (Folha 40)) (Galho 90 (Nada) (Folha 100)))
[50] ++ posOrdem (Galho 30 (Folha 20) (Folha 40)) ++ posOrdem (Galho 90 (Nada) (Folha 100))
[50] ++ [30] ++ posOrdem (Folha 20) ++ posOrdem (Folha 40) ++ [90] ++  posOrdem (Nada) ++  posOrdem (Folha 100)
[50] ++ [30] ++ [20] ++ [40] ++ [90] ++  [] ++  [100]
[50,30,20,40,90,100]

posOrdem (Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) (Galho 20 Nada (Galho 22 (Folha 21) Nada)))
[15] ++ posOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) ++ posOrdem (Galho 20 Nada (Galho 22 (Folha 21) Nada))
[15] ++ [11] ++ posOrdem (Folha 6) ++ posOrdem (Galho 12 (Folha 10) Nada)) ++ [20] ++ posOrdem Nada ++ posOrdem (Galho 22 (Folha 21) Nada)
[15] ++ [11] ++ [6] ++ [12] ++ posOrdem (Folha 10) ++ posOrdem Nada ++ [20] ++ [] ++ [22] ++ posOrdem (Folha 21) ++ posOrdem Nada
[15] ++ [11] ++ [6] ++ [12] ++ [10] ++ [] ++ [20] ++ [] ++ [22] ++ [21] ++ []
[15,11,6,12,10,20,22,21]


--EDR (Esquerda Direita Raiz)
preOrdem :: Arvore -> [Int]
preOrdem Nada = []
preOrdem (Folha x)=[x]
preOrdem (Galho x gEsq gDir)=preOrdem gEsq ++ preOrdem gDir ++ [x]

TESTE DE MESA:
preOrdem (Galho 50 (Galho 30 (Folha 20) (Folha 40)) (Galho 90 (Nada) (Folha 100)))
preOrdem (Galho 30 (Folha 20) (Folha 40)) ++ preOrdem (Galho 90 (Nada) (Folha 100)) ++ [50]
preOrdem (Folha 20) ++ preOrdem (Folha 40) ++ [30] ++ preOrdem (Nada) ++ preOrdem (Folha 100) ++ [90] ++ [50]
[20] ++ [40] ++ [30] ++ [] ++ [100] ++ [90] ++ [50]
[20,40,30,100,90,50]

preOrdem (Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) (Galho 20 Nada (Galho 22 (Folha 21) Nada)))
preOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nada)) ++ preOrdem (Galho 20 Nada (Galho 22 (Folha 21) Nada)) ++ [15]
preOrdem (Folha 6) ++ preOrdem (Galho 12 (Folha 10) Nada)) ++ [11] ++ preOrdem (Galho 20 Nada (Galho 22 (Folha 21) Nada)) ++ [15]
[6] ++ preOrdem (Folha 10) ++ preOrdem Nada ++ [12] ++ [11] ++ preOrdem Nada ++ preOrdem (Galho 22 (Folha 21) Nada) ++ [20] ++ [15]
[6] ++ [10] ++ [] ++ [12] ++ [11] ++ [] ++ preOrdem (Folha 21) ++ preOrdem Nada ++ [22] ++ [20] ++ [15]
[6] ++ [10] ++ [] ++ [12] ++ [11] ++ [] ++ [21] ++ [] ++ [22] ++ [20] ++ [15]
[6,10,12,11,21,22,20,15]

-}