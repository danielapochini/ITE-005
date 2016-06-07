module Aula2 where 


-- !clear - limpeza da tela
-- Dia é chamado de Data Constructor
-- Lado direito são chamados  de Value Constructor
-- :t pergunta o tipo de algo
data Dia = Segunda | Terca | Quarta | Quinta | 
    Sexta | Sabado | Domingo deriving Show
-- deriving Show faz uma conversao, sempre que quiser mostrar um Tipo na tela 

diaDeBalada :: Dia -> Bool 
-- recebe um dia como parametro e retorna um true ou false como resposta
diaDeBalada Sabado = True
diaDeBalada Sexta = True
diaDeBalada _ = False

{- 

Pattern Matching:
Encontra um padrão especifico na(s) entrada(s) da função.
Age de acordo com os values constructors do tipo de entrada.

-} 

numDoDia :: Dia -> Int 
numDoDia Domingo = 1 
numDoDia Segunda = 2
numDoDia Terca = 3
numDoDia Quarta = 4
numDoDia Quinta = 5
numDoDia Sexta = 6
numDoDia Sabado = 7

{-
Faça uma funcao chamada salario que implemente a seguinte regra de negocio:
- Todo trabalhador aos domingos ganha o dobro
- Aos sabados 75% a mais
- Qualquer outro dia nao há acrescimo
A função deve receber um Dia e o valor bruto do salario por hora a ser calculado

-}

salario :: Dia -> Double -> Double 
salario Domingo s = s * 2
salario Sabado s = s * 1.75 
salario _ s = s

{- Faça o tipo Day que possua como value constructors os dias da semana
em Ingles.
Faça as funções traduzirIP que traduz os dias em ingles para portugues
e TraduzirPI que traduz do pt pro ing

-}

data Day = Monday | Tuesday | Wednesday | Thursday | 
    Friday | Saturday | Sunday deriving Show
    
traduzirIP :: Day -> Dia
traduzirIP Monday = Segunda
traduzirIP Tuesday = Terca
traduzirIP Wednesday = Quarta
traduzirIP Thursday = Quinta
traduzirIP Friday = Sexta
traduzirIP Saturday = Sabado 
traduzirIP Sunday = Domingo 


traduzirPI :: Dia -> Day 
traduzirPI Segunda = Monday 
traduzirPI Terca = Tuesday 
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday
traduzirPI Domingo = Sunday 

-- reverse . show . traduzirIP $ Monday 
-- "adnugeS"

-- reverse (show (traduzirIP Monday))
-- "adnugeS"

-- (reverse . show . traduzirIP) Monday
-- "adnugeS"


mult :: (Int, Int) -> Int
-- recebe uma tupla, retorna um inteiro

mult x = fst x * snd x
-- mult (6,7) resultado 42

mult' :: (Int, Int) -> Int 
mult' (x,y) = x * y


{- 

1) Faça uma função que receba um inteiro e eh retornado o dobro - deste inteiro -
na primeira  coordenada de uma tupla, o triplo na segunda e o quadruplo na terceira.



-} 

calcTupla :: Int -> (Int, Int, Int) 
calcTupla x = (2*x, 3*x, 4*x)

{-
2) Faça o tipo Pessoa que pode ser Fisica ou Juridica, e o tipo Imposto que pode
ser ISS, IRPF ou ICMS. Faça a funcao devePagar que recebe uma Pessoa e Imposto.
Esta funcao informa se esta pessoa deve pagar ou nao este Imposto.

-} 

data TipoPessoa = Fisica | Juridica deriving Show
data TipoImposto = ISS | IRPF | ICMS deriving Show    

devePagar :: (TipoPessoa, TipoImposto) -> Bool
devePagar (Fisica, IRPF) = True
devePagar (Juridica, ISS) = True
devePagar (Juridica, ICMS) = True
devePagar _ = False 