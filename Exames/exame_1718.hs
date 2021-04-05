module Exame where 

import Data.List
import Data.Char


--1 

lista1 = [0,1,2,2,8,7,9,29] 

posiçaoele :: [a] -> Int -> a
posiçaoele (a:as) 0 = a 
posiçaoele (a:as) k = posiçaoele as (k-1)



--2

data Movimento = Norte | Sul | Este | Oeste deriving Show

--posiçao inicial (x,y) , depois vou adicionar lista de movimentos para norte
-- dá mais y e para este dá mais x
-- norte +1y    este +1x

{- 

instance Eq Movimento where
    m1 == m2 = 




posicao :: Eq => (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (m:ms) |m == Norte = posicao (a,b+1) ms
                     |m == Este  = posicao (a+1,b) ms   


-}


posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (Norte:m) = posicao (a,b+1) m
posicao (a,b) (Este:m)  = posicao (a+1,b) m


--3

psany :: (a->Bool) -> [a] -> Bool
psany p [] = False
psany p (h:t) = p h || psany p t







--4 

-- a ideia é fazer takewhile 0 e contar a lenght depois a lenght dos em baixo
-- tem de ser sempre maior que o de cima...

matriz1 = [[1,2,3], [2,3,4] , [8,9,0]] 
matriz2 = [[1,2,3] ,[0,2,4] , [0,0,9]]



{-

type Mat a = [[a]]
triSup :: Eq a => Ord a => Num a => Mat a -> Bool
triSup [] = True
triSup [a] = True
triSup ((a:as):(b:bs):c) |length (takeWhile (==0) (a:as)) < length (takeWhile (==0)  (b:bs)) 
                          = triSup ((b:bs):c) 
                         |otherwise = False 



-}


{-



type Mat a = [[a]]
triSup :: Eq a => Ord a => Num a => Mat a -> Bool
triSup [] = True
triSup [a] = True
triSup ((a:as):(b:bs):c) |length (dropWhile (==0) (a:as)) > length (dropWhile (==0)  (b:bs)) 
                          = triSup ((b:bs):c) 
                         |otherwise = False 

-}



type Mat a = [[a]]
triSup :: Eq a => Ord a => Num a => Mat a -> Bool
triSup [] = True
triSup [a] = True
triSup ((a:as):(b:bs):c) |length x > length y = triSup ((b:bs):c) 
                         |otherwise = False 

                       where
                        x = dropWhile (==0) (a:as)
                        y = dropWhile (==0) (b:bs)
                         



--5

movimenta :: IO (Int,Int)
movimenta = do putStrLn "inserir letra"
               l <- getChar
               let verdirecao = posicao2 l 
               return verdirecao








posicao2 :: Char -> (Int,Int)
posicao2 a |a== 'n' = (0,1)
           |a== 's' = (0,-1)
           |a== 'e' = (1,0)
           |a== 'o' = (-1,0)























--6

data Imagem = Quadrado Int
                            | Mover (Int,Int) Imagem
                            | Juntar [Imagem]


                            