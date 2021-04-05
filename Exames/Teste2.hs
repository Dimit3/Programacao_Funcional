module Teste2 where

--1
insert :: Ord a => a -> [a] -> [a]
--insert 25 [1,20,30,40] = [1,20,25,30,40]
insert x [] = [x]
insert x (h:t) | x <= h = x:h:t
               | otherwise = h: insert x t

--2
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:t) = x: catMaybes t
catMaybes (Nothing:t) = catMaybes t

--3
data Exp a = Const a
            | Var String
            | Mais (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)
--show (Mais (Var "x") (Mult (Const3) (Const 4))) = "(x + (3 * 4))"

instance (Show a ) => Show (Exp a) where
    show (Const n) = show n
    show (Var x) = x
    show (Mais y z) = "(" ++ show y ++ " + " ++ show z ++ ")"
    show (Mult y z) = "(" ++ show y ++ " * " ++ show z ++ ")"

--4
sortOn :: Ord b => (a -> b) -> [a] -> [a]
--sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]
sortOn f [] = []
sortOn f (h:t) = sortOnA f h (sortOn f t)
{-sortOn f l = sortOnB f l []

sortOnB :: Ord b => (a -> b) -> [a] -> [a] -> [a]
sortOnB f [] l = l
sortOnB f (h:t) l = sortOnB f t (sortOnA f h l)-}


sortOnA :: Ord b => (a -> b) -> a -> [a] -> [a]
sortOnA f x [] =  [x]
sortOnA f x (h:t) | f x <= f h = x:h:t
                  | otherwise = h: sortOnA f x t

--5
--a)
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = (maximum l) - (minimum l)

--b)
--parte :: [Int] -> ([Int],[Int])
--parte [1,18,3,19,17,20] == ([2,3],[resto]) || (,[2,3])
--parte 

--6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])

--a
conta :: Imagem -> Int
--conta ex == 3
conta (Quadrado x) = 1
conta (Mover (x,y) n) = conta n
conta (Juntar (h:t)) = conta h + conta (Juntar t)
conta (Juntar []) = 0