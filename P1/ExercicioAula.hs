module ExercicioAula where

{-- Exercício 1.7 
Sabe-se que as unidades imperiais de comprimento podem ser Inch, Yard ou Foot. 
Sabe-se que 1 inch (polegada) = 0.0254m, 1 yard (jarda) = 0.9144m, 1 foot (pés) = 0.3048m. --}

-- D.C Unidade e V.C com 4 campos 
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