module Ficha4 where 
import Data.Char
import Data.List
import Data.Either



--1
--

div2e3 = [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

div2e33 = [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

huh2 = [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

huh1 = [sum [y | y <- [1..x], odd y] | x <- [1..10]]

--2
--[1,2,4,8,16,32,64,128,256,512,1024]
--a

lista1 = [ 2 ^ x | x <- [1..10]] 

--b 
lits2 = [(x,6-x) | x <- [1..5]] 

--c
lista3 = [[1..x] | x <- [1..5]] 


--d 

lista4 = [take x [1,1..] | x <- [1..5]] 




type Nome = String 
type Agenda = [(Nome,[Contacto])]
data Contacto = Casa Integer
              | Trab Integer
              | Email String
              | Tlm Integer
              deriving Show

--3
emails :: [Contacto] -> [String]
emails [] = []
emails (Email x : t) = x : emails t
emails (_ : t) = emails t

--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n email [] = [(n, [Email email])]
acrescEmail n email ((a,b):t) | n == a = (a, Email email : b) : t
                              | otherwise = (a,b) : acrescEmail n email t

--b)
{-
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((a,b):t) | x == nome = Just (emails b)
                         | x /= nome = verEmails n t



--c)
casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ((x,c):t) | x == nome = casa x c
                    | x /= nome = casa nome t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs c = 

-}