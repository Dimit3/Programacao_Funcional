module Main where

main :: IO ()




{-


main = do putStrLn "ficheiro"
          f <- getLine
          x <- readFile f
          let l = length ( lines x) 
          let w = length ( words x)
          let c = length x 
          putStrLn ("linhas:" ++ (show l))
          putStrLn ("palavras:" ++ (show w))
          putStrLn ("caracteres:" ++ (show c)) 



-}


main = do putStrLn "Escrever uma frase:"
          putStrLn ""
          f <- getLine
          let x = length ( lines f) 
          let w = length ( words f)
          let c = length f
          putStrLn "" 
          putStrLn ("linhas:" ++ (show x))
          putStrLn ("palavras:" ++ (show w))
          putStrLn ("caracteres:" ++ (show c)) 
          putStrLn ""
          putStrLn ""
          putStrLn "Continuar? sim / não"
          f2 <- getLine 
          case aux f2 "sim" of 
                      True -> main
                      False-> putStrLn "Acabamos"




aux :: Eq a => [a] -> [a] ->  Bool
aux [] _  = True
aux _ []  = False 
aux (x:xs) (y:ys) |x == y = aux xs ys
                  |otherwise = False  