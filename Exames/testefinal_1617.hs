module Testefinal1617 where 

import System.Random
import Data.List
import Data.Char



--1 

type MSet a = [(a,Int)]
conjuntoA = [('b',4),('a',2),('c',1)]


-- O multi-conjunto {’b’,’a’,’c’,’b’,’b’,’a’,’b’} 
-- é a lista [(’b’,4),(’a’,2),(’c’,1)]

--a 

cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet ((a,k):b) = k + cardMSet b 



--b 

-- moda :: MSet a -> [a] 







--c
--converteMSet [(’b’,4),(’a’,2),(’c’,1)] devolve ‘‘bbbbaac’’.



converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((y,n) : ys) = aux (y,n) ++ converteMSet ys
                       where  aux (y,0) = []
                              aux (y,n) = y : aux (y,n-1) 



--d
-- addNcopies [('b',4),('a',2),('c',1)] 'd' 10 = [(’b’,14),(’a’,2),(’c’,1)]

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies m x k = (addNcopies1 m x k)



addNcopies1 :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies1 [] x k = [(x,k)]
addNcopies1 ((a,b):as) x k |a== x = ((a,b+k):as) 
                           |otherwise = (a,b): addNcopies1 as x k 


--funçao ordenar mset
{- 
msetord:: Ord a => MSet a -> MSet a
msetord [] = [] 
msetord ((a,k),(a1,k1):t) |k >= k1   = (a,k)   : (msetord (a1,k1) :t) 
                          |otherwise = (a1,k1) : (msetord (a,k):t) 

-}

--2 
--a 


data SReais = AA Double Double | FF Double Double
              | AF Double Double | FA Double Double
              | Uniao SReais SReais




instance Show SReais where
        show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
        show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
        show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"
        show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
        show (Uniao a b) = show a ++ " U " ++ show b  




pertence :: Double-> SReais -> Bool 
pertence x (AA y z) =  if (x > y) && (x < z)  then True else False  
pertence x (FA y z) =  if (x >= y) && (x < z) then True else False 
pertence x (AF y z) =  if (x > y) && (x <= z) then True else False 
pertence x (FF y z) =  if (x >= y) && (x <= z)then True else False  



tira :: Double -> SReais -> SReais
tira x (AA y z) = (Uniao (AA y x) (AA x z)) 
tira x (AF y z) = (Uniao (AA y x) (AF x z)) 
tira x (FA y z) = (Uniao (FA y x) (AA x z)) 
tira x (FF y z) = (Uniao (FA y x) (AF x z)) 




--3

data RTree a = R a [RTree a]


--a 









--b

pr