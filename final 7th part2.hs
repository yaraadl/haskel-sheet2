--yara adel hassan mohamed 
--19100683
--7th assignmnet spl
---------------------------------------------
--final answer for question 1
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap p q [] = []
altMap p q [x] = [p x]
altMap p q (x : y : xs) = p x : q y : altMap p q xs

-------------------------------------------------
--final answer for question 2
palProducts::  Int -> Int -> [Int]
rmdups :: [Int] -> [Int]
palprod ::  Int -> Int -> [Int]
isPalindrome :: Int -> Bool 
digits :: Int -> [Int]
palProducts n p =rmdups[x*y|x<-[n..p],y<-[n..p]]

rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

isPalindrome w = z == reverse z
                 where z = digits w 
                 

palprod n p = [x | x <- (palProducts n p ) ,isPalindrome x]
------------------------------------------------------------------------------------------
--final answer question 3

isograms ::  [[Char]] -> [[Char]]
rmdups ::   [Char] -> Bool
rmdups (x:[]) = True
rmdups (x:xs)   | x `elem` xs   = False
                | (x == ' ') || (x == '-') = rmdups xs
                | otherwise     =  rmdups xs
isograms xs = [x|x<-xs, (rmdups x)]
-----------------------------------------------------------------------------------------
--final answer question 4
import Data.List (delete)
anagrams :: Eq a => [a]-> [[a]] -> [[a]]
permutations :: Eq a => [a] -> [[a]]

permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (delete x xs)]

anagrams x xs = [y | y<-xs, y `elem` (permutations x) ] 
--------------------------------
--final answer question 5
module Triangle (rows) where
rows :: Int -> [[Integer]]
rows n = take n pascal where
  pascal = [1] : map next pascal
  next r = zipWith (+) (0:r) $ r++[0]
---------------------------------------------------------------------------

