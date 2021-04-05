module Exame1617 where



--Por exemplo, unlines ["Prog", "Func" == "Prog\nFunc".


myunlines :: [String] -> String
myunlines [] = ""
myunlines (a:as) = a ++ "\n" ++ myunlines as 



-- (\\) [1,2,3,4,5,1] [1,5] == [2,3,4,1] 
-- (\\) [1,2,2,3,2,1,4,1] [2,1,2] == [3,2,1,4,1]


semprimeiros :: (Eq a) => [a] -> [a] -> [a]
semprimeiros a [] = a 
semprimeiros [] _ = [] 
semprimeiros (a:as) (b:bs) | a == b = semprimeiros as bs
                           |otherwise = a : semprimeiros as (b:bs)





data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a


primeiro :: Seq a -> a
primeiro (Inicio a b) = a 
primeiro (Fim b a)    = primeiro b 



semUltimo :: Seq a -> Seq a
semUltimo (Fim Nil a)  = Nil 
semUltimo (Fim a  b )  = semUltimo a
semUltimo (Inicio a b) = semUltimo b 




data BTree a = Empty | Node a (BTree a) (BTree a) 
                         deriving Show 

arv1 = Node 2 Empty (Node 6 Empty Empty ) 


prune :: Int -> BTree a -> BTree a
prune 0 a = Empty 
prune x (Node a e d) = Node a (prune (x-1) e) (prune (x-1) d)




semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node x Empty r ) = r  
semMinimo (Node x e d) = Node  x (semMinimo e) d







--4 

type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

--  exemplo == [(2,0),(0,1),(3,2),(1,3)].



posicoes:: Tabuleiro -> [(Int,Int)] 
posicoes a = posicoes1 0 a 



posicoes1:: Int -> Tabuleiro -> [(Int,Int)] 
posicoes1 _ [] = [] 
posicoes1 x (a:as) = somalc (posicoesauxc a (0,0)) (0,x) : posicoes1 (x+1) as



posicoesauxc :: String -> (Int,Int) -> (Int,Int)  
posicoesauxc [] (c,l) = (c,l)
posicoesauxc (a:as) (c,l) |a == 'R' =  (c,l) 
                          |otherwise = posicoesauxc as (c+1,l) 



somalc :: (Int,Int) -> (Int,Int) -> (Int,Int) 
somalc (x1,y1) (x2,y2) = (x1+x2,y1+y2)




