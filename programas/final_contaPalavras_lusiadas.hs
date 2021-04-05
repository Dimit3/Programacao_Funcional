module Main where 

import Data.Char
import System.Random
import Data.List




main :: IO ()


  
main = do l <- readFile "C:\\Users\\Dimitri\\Desktop\\lusiadas.txt"
          let palavras = map minusc (words l)
          let c = controiConta palavras 
          let resultado = mv c 
          let resultado2 = mr c
          putStrLn resultado
          putStrLn resultado2





type Conta = [(String,Int)]



inserec :: String -> Conta -> Conta
inserec a [] = [(a,1)]
inserec a ((b,c):d) | a == b = ((b,c+1) : d) 
                    |otherwise = (b,c) : inserec a d  



controiConta :: [String] -> Conta
controiConta [] = []
controiConta (a:as) = inserec a (controiConta as) 



maisvezes :: Conta -> (String,Int)
maisvezes [(a,b)] = (a,b)
maisvezes ((a,b):(c,d):e) |b > d = maisvezes ((a,b): e)
                          |otherwise  = maisvezes ((c,d): e)


mv :: Conta -> String
mv c = "palavra: " ++ show (fst (maisvezes c)) 


mr :: Conta -> String
mr c = "numero de vezes que aperece: " ++ show (snd (maisvezes c)) 

minusc :: String -> String 
minusc a  = map toLower a 




