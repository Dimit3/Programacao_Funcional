module Testefinal1718 where




-- Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40].
--1



insert :: Ord a => a -> [a] ->[a]
insert a [] = [a] 
insert a (b:bs) |a > b = b : insert a bs 
                |otherwise = a:(b:bs)





--2

catMaybes :: [Maybe a] -> [a]
catMaybes (Just a : b) = a : catMaybes b
catMaybes (Nothing :b) = catMaybes b
catMaybes [] = []




--3 


data Exp a = Const a 
             |Var String   
             |Mais (Exp a) (Exp a) 
             |Mult (Exp a) (Exp a)


--show (Mais (Var "x") (Mult (Const3) (Const 4))) = "(x + (3 * 4))".



instance Show a => Show (Exp a) where
     show (Const a) = show a 
     show (Var a )  = a
     show (Mais x y) = show x ++ " + " ++ show y
     show (Mult x y) = show x ++ " * " ++ show y




--4


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = sortOnA f h (sortOn f t)


sortOnA :: Ord b => (a -> b) -> a -> [a] -> [a]
sortOnA f x [] =  [x]
sortOnA f x (h:t) | f x <= f h = x:h:t
                  | otherwise = h: sortOnA f x t


--5 

--a
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = maximum l - minimum l 


--b

-- parte :: [Int] -> ([Int],[Int])





--6


data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

--a
{- 

conta:: Imagem -> Int
conta a = conta1 0 a 


conta1 ::Int -> Imagem -> Int  
conta1 k (Quadrado a) = (k+1) 
conta1 k (Mover (x,y) a) = conta1 k a  
conta1 k (Juntar (a:as)) = conta1 k a + conta1 k (Juntar as)
conta1 k (Juntar []) = 0

-}

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])

--a
conta :: Imagem -> Int
--conta ex == 3
conta (Quadrado x) = 1
conta (Mover (x,y) n) = conta n
conta (Juntar (h:t)) = conta h + conta (Juntar t)
conta (Juntar []) = 0