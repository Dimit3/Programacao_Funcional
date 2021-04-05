module Ficha1 where 

--exercicio 1
--a
perimetro r=2 * pi * r
 
--b
dist:: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt( (x2 - x1) ^ 2 + (y2 - y1) ^ 2 )

--c
primUlt:: [Int] -> (Int,Int)
primUlt l = (head l,last l)

--d
multiplo:: Integer -> Integer -> Bool
multiplo m n = if (mod m n == 0) then True else False

--e 
truncaImpar:: [a] -> [a]
truncaImpar l = if (mod (length l) 2 ) == 0
                then l
                else tail l 


--f
max2:: (Float) -> (Float) -> (Float)
max2 x y = if x > y then x else y

--g 
{- max3:: (Float) -> (Float) -> (Float) -> (Float)
max3 x y z = if z > max2 x y then z else max2 x y -}

max3:: (Float) -> (Float) -> (Float) -> (Float)
max3 x y z = max2 z (max2 x y)



--2
-- a   A funcao nRaizes que recebe os (3) coeficientes de um polinomio de 2o grau e que
--     calcula o numero de raızes (reais) desse polinomio.

nRaizes:: (Float) -> (Float) -> (Float) -> (Integer)
nRaizes a b c 
             |b^2 - 4 *a * c == 0 = 1
             |b^2 - 4 *a * c > 0  = 2
             |otherwise = 0 


--b
--calcula as raizes
raizes:: (Float) -> (Float) -> (Float) -> [Float]
raizes a b c 
            |nRaizes a b c == 0 = []
            |nRaizes a b c == 1 = [ - b / 2 * a ]
            |nRaizes a b c == 2 = let delta = b^2 - 4 *a * c
                                      r = sqrt delta
                            in [((-b) + r)/(2 * a),((-b) - r / (2 * a))]

--3
--a

type Hora = (Int,Int)
validohora:: Hora -> Bool 
validohora (hora,min) = if hora > 0 && hora < 24 && min > 0 && min < 60 then True else False

--b
--hora depois da outra 

horadepois:: Hora -> Hora -> Bool
horadepois (h1,m1) (h2,m2) = if h1 > h2 then True else if m1 > m2 then True else False

--c 
--converter hora em min

converterhora:: Hora -> Int
converterhora (h1,m1) = h1 * 60 + m1 

--d 
converterminutos:: Int -> Hora
converterminutos x = (div x 60 , mod x 60)

--e 
difhoras:: Hora -> Hora -> Int
difhoras (h1,m1) (h2,m2) = (converterhora (h1,m1) - converterhora (h2,m2)) 

--f 
adicionarmin:: Hora -> Int -> Hora
adicionarmin h x = converterminutos (converterhora h + x )

--5
data Semaforo =  Verde | Amarelo | Vermelho deriving (Show,Eq)

nextsemaforo:: Semaforo -> Semaforo
nextsemaforo Verde = Vermelho
nextsemaforo Vermelho = Verde
nextsemaforo Amarelo = Vermelho



stopsemaforo:: Semaforo -> Bool
stopsemaforo Vermelho = True
stopsemaforo Verde = False
stopsemaforo Amarelo = False

safesemaforo:: Semaforo -> Semaforo -> Bool
safesemaforo Vermelho _ = True
safesemaforo _ Vermelho = True
safesemaforo _ _ = False
