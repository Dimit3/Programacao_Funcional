module Perguntasteste where



import Data.Char
import Data.List
import Data.Either



--1 funçao recursiva que constroi lista numeros inteiros 
--Por exemplo, enumFromTo 1 5 corresponde `a lista [1,2,3,4,5]

myenumFromTo:: Int -> Int -> [Int]
myenumFromTo a b 
               | a > b = []
               |otherwise = a : myenumFromTo (a+1) b




--2
--Por exemplo, enumFromThenTo 1 3 10 corresponde `a lista [1,3,5,7,9].

myenumFromThenTo :: Int -> Int -> Int -> [Int] 
myenumFromThenTo x y z | x<y && x<z = x:myenumFromThenTo y (y+(y-x)) z
                       | x==z       = [x]
                       | x>y && x>z = x:myenumFromThenTo y (y-(x-y)) z
                       | otherwise  = []


--3
--Por exemplo, (++) [1,2,3] [10,20,30] corresponde `a lista [1,2,3,10,20,30].

myconcatena::[a] -> [a] -> [a] 
myconcatena l [] = l
myconcatena [] l = l 
myconcatena (x:xs) y = x : myconcatena xs y

--4
--Por exemplo, (!!) [10,20,30] 1 corresponde a 20.

myindex::[Int] -> Int -> Int
myindex (a:as) 0 = a
myindex (a:as) n = myindex as (n-1) 

--5
--Por exemplo, reverse [10,20,30] corresponde a [30,20,10].
myreverse::[Int] -> [Int] 
myreverse [] = [] 
myreverse (a:as) = myreverse (as) ++ [a]

--6 
--Por exemplo, take 2 [10,20,30] corresponde a [10,20].
mytake::Int -> [a] -> [a] 
mytake 0 a = [] 
mytake x []  = [] 
mytake x (a:as) = a : mytake (x-1) as

--7
--Por exemplo, drop 2 [10,20,30] corresponde a [30].
mydrop::Int -> [a] -> [a]
mydrop 0 l = l
mydrop x []= []
mydrop x (l:ls) = mydrop (x-1) ls 


--8
--zip
myzip:: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = [] 
myzip (a:as) (b:bs) = (a,b) : myzip as bs

--9
--elem testa se elemento aparece na lista
myelem:: Eq a => a -> [a] -> Bool
myelem x [] = False 
myelem x (a:as) |x == a = True
                |otherwise = myelem x as 

--10 
-- ex replicate 3 10 corresponde a [10,10,10]
myreplicate :: Int -> a ->[a]
myreplicate 0 a = [] 
myreplicate x a = a : myreplicate (x-1) a 

--11
--dado um elemento e uma lista, constroi uma lista em que o elemento fornecido e 
--intercalado entre os elementos da lista fornecida.
--Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].

myintersperse :: a -> [a] -> [a] 
myintersperse x [] = [] 
myintersperse x (a:as) = a : x : myintersperse x as 


--12
--Por exemplo, mygroup [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].
--agrupa elementos
 

mygroup :: Eq a => [a] -> [[a]]
mygroup (h:t) = aux [h] t 
                where
                 aux a [] = [a]
                 aux a (h:t) |elem h a  = aux (h:a) t 
                             |otherwise = a : aux [h] t 


--13
--Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].
myconcat::  [[a]] -> [a]  
myconcat [] = []
myconcat (a : as) = a ++ myconcat as 






--14
--Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]

myinits::[a] -> [[a]] 
myinits [] = [[]]
myinits l = myinits (init l) ++ [l] 




--15
--Por exemplo, mytails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].

mytails:: [a] -> [[a]]
mytails [] = [[]]
mytails l  = [l] ++ mytails (tail l) 




--16
--Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
--[10,30] [10,20,30] corresponde a False.

myisPrefixOf :: Eq a => [a] -> [a] -> Bool 
myisPrefixOf [] _ = True
myisPrefixOf _ [] = True
myisPrefixOf (a:as) (b:bs) |a == b && myisPrefixOf as bs = True 
                           |otherwise = False 



--17
--Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
--[10,30] [10,20,30] corresponde a False.

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True 
myisSuffixOf x y  = if last x == last y 
                  then myisSuffixOf ( init x) (init y )
                  else False





--18
--Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que
--isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.


myisSubsequenceOf :: Eq a =>[a] -> [a] -> Bool 
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False 
myisSubsequenceOf (a:as) (b:bs) =if a==b 
                               then myisSubsequenceOf as bs
                               else myisSubsequenceOf (a:as) bs 


--19
--Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6].

myelemindices:: Eq a => a -> [a] -> [Int]
myelemindices x l = myelemindicesA 0 x l 


myelemindicesA:: Eq a => Int -> a -> [a] -> [Int]
myelemindicesA k a [] = []
myelemindicesA k x (a:as) |x== a = k : myelemindicesA (k+1) x as 
                          |otherwise = myelemindicesA (k+1) x as  





myelemindicesb:: Eq a => a -> [a] -> [Int]
myelemindicesb x [] = []
myelemindicesb x (h:t) |x== h = 0 : l 
                       |otherwise = l
                   where e = myelemindicesb x t 
                         l = [x+1 | x<-e] 






--20
--Por exemplo, mynub [1,2,1,2,3,1,2] corresponde a [1,2,3].


mynub:: Eq a => [a] -> [a] 
mynub [] = [] 
mynub (a:as) = a: mynub (aus a as)
            where 
                aus _ [] = []
                aus x (y:ys) = if x==y 
                               then aus x ys
                               else y: aus x ys 





--21
--Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se n˜ao existir nenhuma
--ocorrˆencia a fun¸c˜ao dever´a retornar a lista recebida.

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete x (a:as) |x== a = as 
                  |otherwise = a : mydelete x as  

--22 
--Por exemplo, mydelete2 [1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1] 
mydelete2:: Eq a => [a] -> [a]-> [a] 
mydelete2 []  _  = []
mydelete2 [a] [] = [a]
mydelete2 (a:as) (b:bs) |a == b = mydelete2 as bs
                        |otherwise = a : mydelete2 as (b:bs) 


--23
-- Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5].
myunion:: Eq a => [a] -> [a] -> [a] 
myunion [] a = a
myunion a [] = a
myunion (a:as) (b:bs) |a == b = a : myunion as bs
                      |otherwise = a : b : myunion as bs 




--24 
--Por exemplo, myintersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3]
myintersect :: Ord a => [a] -> [a] -> [a]
myintersect  a [] = [] 
myintersect []  a = [] 
myintersect (a:as) l |elem a l  = a: myintersect as l 
                     |otherwise = myintersect as l 



--25
--Por exemplo, myinsert 25 [1,20,30,40] corresponde a [1,20,25,30,40]
myinsert::  Ord a => a -> [a]-> [a]
myinsert a [] = [a]
myinsert a (b:bs) |a <= b = a : b : bs
                  |a >  b = b: myinsert a bs 





--26
--Por exemplo, myunwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional"

myunwords :: [String] -> String
myunwords [a] = a 
myunwords []  = ""
myunwords (a:as) = a ++ [' '] ++  myunwords as 


--27
--Por exemplo, myunlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".

myunlines :: [String] -> String
myunlines [a] = a ++ "\n"
myunlines []  = ""
myunlines (a:as) = a ++ ['\n'] ++  myunlines as 


--28
--numero do maior elemento começa em 0


pMaior ::Ord a => [a] -> Int
pMaior [x] = 0
pMaior (a:as) |a == aus (a:as) = 0
              |otherwise = 1 + pMaior as
             where
                aus [x] = x 
                aus (x:y:xs) | x >= y = aus (x:xs) 
                             | otherwise = aus (y:xs)




--29 
-- Por exemplo, temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos
-- [11,2,31,4] corresponde a False.
-- testa se tem repetidos


mytemrepetidos::  Eq a => [a] -> Bool
mytemrepetidos [] = False
mytemrepetidos (a:as) = elem a as || mytemrepetidos as


--30
--Por exemplo, algarismos "123xp5" corresponde a "1235".


myalgarismos:: [Char] -> [Char]
myalgarismos [] = [] 
myalgarismos (a:as) |isDigit a  = a:myalgarismos as 
                    |otherwise  = myalgarismos as 


--31
--Por exemplo, myposImpares [10,11,7,5] corresponde a [11,5].

myposImpares :: [a] -> [a]
myposImpares [] = []
myposImpares [a]     = []
myposImpares (a:y:as) = y : myposImpares as 


--32 
--Por exemplo, myposPares [10,11,7,5] corresponde a [10,7].

myposPares :: [a] -> [a] 
myposPares [] = []
myposPares [a]     = [a]
myposPares (a:y:as) = a : myposPares as 


--33
--Por exemplo, myisSorted [1,2,2,3,4,5] corresponde a True, enquanto que 
--myisSorted [1,2,4,3,4,5] corresponde a False.

myisSorted :: Ord a => [a] -> Bool
myisSorted []  = True
myisSorted [x] = True
myisSorted (a:b:as) |a <= b = myisSorted (b:as)
                    |otherwise = False


--34
--calcula o resultado de ordenar uma lista
--insert :: Ord a => a -> [a] -> [a]

lista1 = [1,2,3,4,3,2,3,4,5] 

myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort (a:as) = insert a (myiSort as) 

--35
--Por exemplo, mymenor "sai" "saiu" corresponde a True enquanto que 
--mymenor "programacao" "funcional" corresponde a False.

mymenor :: String -> String -> Bool
mymenor [] _ = True
mymenor _ [] = False 
mymenor (a:as) (b:bs) |ord a > ord b = False
                      |ord a < ord b = True 
                      |otherwise = mymenor as bs


--36 
-- Por exemplo, myelemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True
-- myelemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False. 


myelemMSet :: Eq a => a -> [(a,Int)] -> Bool
myelemMSet _ [] = False 
myelemMSet x ((a,as):t) |x==a = True 
                        |otherwise = myelemMSet x t  



--37
--Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7
--Defina a fun¸c˜ao lengthMSet :: [(a,Int)] -> Int que calcula o tamanho de um multiconjunto

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h:t) = snd h + lengthMSet t

--38
--Por exemplo, myconverteMSet [('b',2), ('a',4), ('c',1)] corresponde a "bbaaaac".


myconverteMSet :: [(a,Int)] -> [a]
myconverteMSet [] = []
myconverteMSet ((y,n) : ys) = aux (y,n) ++ myconverteMSet ys
                       where  aux (y,0) = []
                              aux (y,n) = y : aux (y,n-1) 


--39
--Defina a fun¸c˜ao insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
--um elemento a um multi-conjunto.
--Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
--(’a’,4), (’c’,2)].


myinsereMset:: Eq a => a -> [(a,Int)] -> [(a,Int)]
myinsereMset x [] = [(x,1)]
myinsereMset x ((a,n):as) |x==a = ((a,n+1):as)
                          |otherwise = (a,n) : myinsereMset x as  



--40
--Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
-- (’a’,4)].


myremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myremoveMSet _ [] = []
myremoveMSet x ((y,n):ys) |(x==y) && (n>1) = ((y,n-1) :ys) 
                          |(x==y) && (n==1) = ys
                          |otherwise = (y,n) : myremoveMSet x ys 


--41
--Por exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)].

myconstroiMSet :: Ord a => [a] -> [(a,Int)]
myconstroiMSet [] = []
myconstroiMSet l  = aux 1 l 
                 where 
                    aux i [x] = [(x,i)]
                    aux i (x:y:xs) |x == y    =  aux (i+1) (x:xs) 
                                   |otherwise =  (x,i) : aux 1 (y:xs) 


--42
--divide uma lista de Either s em duas listas.

mypartitionEithers :: [Either a b] -> ([a],[b]) 
mypartitionEithers l = (left l, right l)
                    where
                      left  (Left a :t)   = a : left t
                      left  (Right b :t)  = left t
                      left _  = []
                      right (Left a :t)   = right t
                      right (Right a :t)  = a : right t
                      right _ = []





--43
--Apresente uma defini¸c˜ao recursiva da fun¸c˜ao pr´e-definida catMaybes :: [Maybe a] -> [a]
--que colecciona os elementos do tipo a de uma lista.

mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = [] 
mycatMaybes (Just a : as)   = a : mycatMaybes as
mycatMaybes (Nothing : as)  = mycatMaybes as 


--44
--dada uma posi¸c˜ao inicial (coordenadas) e uma lista de movimentos, 
--calcula a posi¸c˜ao final do robot depois de efectuar essa sequˆencia de movimentos.




data Movimento = Norte | Sul | Este | Oeste  deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y) 
posicao (x,y) (Norte : m) = posicao (x,y+1) m
posicao (x,y) (Sul   : m) = posicao (x,y-1) m 
posicao (x,y) (Este  : m) = posicao (x+1,y) m
posicao (x,y) (Oeste : m) = posicao (x-1,y) m



--45
--dadas as posi¸c˜oes
--inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes 
--para que o robot passe de uma posi¸c˜ao para a outra.

caminho:: (Int,Int) -> (Int,Int) -> [Movimento] 
caminho (x1,y1) (x2,y2) |x1==x2 && y1==y2 = []
                        |y1 < y2 = [Norte] ++ caminho (x1,y1+1) (x2,y2)
                        |y1 > y2 = [Sul  ] ++ caminho (x1,y1-1) (x2,y2)
                        |x1 < x2 = [Este ] ++ caminho (x1+1,y1) (x2,y2)
                        |x1 > x2 = [Oeste] ++ caminho (x1-1,y1) (x2,y2)
     

--46
-- so movimentos verticais ou nao 


vertical :: [Movimento] -> Bool
vertical [] = True 
vertical (Norte:as) = vertical as
vertical (Sul  :as) = vertical as
vertical (Este :as) = False
vertical (Oeste:as) = False


--47
--posicao mais central 



data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:t) | dist x <= dist y = maisCentral (x:t)
                    | dist x >  dist y = maisCentral (y:t)


                   where 
                    dist (Pos k m) = (sqrt(fromIntegral(k+m)))^2

--48
--selecciona da lista as posi¸c˜oes adjacentes `a posi¸c˜ao dada.


vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos z w):t) = if (y == w && x == (z+1)) || (y == w && x == (z-1)) 
                                   || (x == z && y == (w+1)) || (x == z && y == (w-1))
                                   then (Pos z w) : vizinhos (Pos x y) t
                                   else  vizinhos  (Pos z w) t


--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [x] = True 
mesmaOrdenada ((Pos x y):(Pos w z):t) = if y == z 
                                        then mesmaOrdenada ((Pos w z):t) 
                                        else False


--50 



data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool 
interseccaoOK l = (aux l) <= 1 
              where
                aux [Vermelho] = 0 
                aux [Verde] = 1
                aux [Amarelo] = 1
                aux (Vermelho:t) = aux t
                aux (Verde:t) = 1+aux t
                aux (Amarelo:t) = 1+aux t