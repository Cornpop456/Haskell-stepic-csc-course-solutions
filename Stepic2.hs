module Stepic2 where

import Data.Char

import Data.List.Split

import Text.Read

import Data.List

-- Lists

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y l = x : y : l


nTimes :: (Eq t, Num t) => a -> t -> [a]
nTimes el n = helper el n [] where 
    helper _ 0 acc = acc
    helper el' n' acc = helper el (n' - 1)  (el' : acc)


oddsOnly :: Integral a => [a] -> [a]
oddsOnly l = filter (\x -> odd x) l


isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == (reverse l)


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a:as) (b:bs) (c:cs) = (a+b+c) : sum3 as bs cs
sum3 [] [] [] = []
sum3 a b c = sum3 (f a) (f b) (f c) where {f [] = [0]; f arr = arr}


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = let (g,r) = collect [x] x xs
  in g : groupElems r
  where
    collect a _ [] = (a, [])
    collect a l (x:xs) | l == x = collect (x : a) x xs
                       | otherwise = (a, x:xs)


readDigits :: String -> (String, String)
readDigits "" = ("", "")
readDigits x | (readMaybe x :: Maybe Int) == Nothing && (not $ isDigit (head x))  = ("", x)
readDigits x = (d, s) where 
    p = head ((reads x) :: [(Int, String)])
    d =  show $ fst  p
    s = snd p

-- readDigits :: String -> (String, String)
-- readDigits = span isDigit


filterDisj :: (t -> Bool) -> (t -> Bool) -> [t] -> [t]
filterDisj p1 p2 l = filter (\x -> p1 x || p2 x) l


qsort :: Ord a => [a] -> [a]
qsort (x:xs) = qsort (filter (<x) xs) ++ (x : qsort (filter (>=x) xs))
qsort [] = []


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes l = concat $ map (\x -> [(^2) x, ((^3) x)]) l


delAllUpper :: [Char] -> [Char]
delAllUpper str =  let 
    target = map (\y -> y ++ " ") $ filter (\x -> length (filter (not . isUpper) x) /= 0) (splitOn " " str)
    res = concat target
                   in take ((length res) - 1) res

-- delAllUpper :: String -> String
-- delAllUpper = unwords . filter (not . all isUpper) . words


max3 :: Ord b => [b] -> [b] -> [b] -> [b]
max3 l1 l2 l3 = map (\x -> maxHelp x) $ zip3 l1 l2 l3

maxHelp :: Ord a => (a, a, a) -> a
maxHelp (a,b,c) = max a (max b c)

-- max3 :: Ord a => [a] -> [a] -> [a] -> [a]
-- max3 a b c = zipWith max c (zipWith max a b)

  
fibStream :: [Integer]
fibStream = helper 0 1 
    where 
        helper a b = a : helper b (a + b)


repeat' :: a -> [a]
repeat' = iterate repeatHelper
repeatHelper :: a -> a
repeatHelper = id


combinations :: Integer -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)


coins ::(Ord a, Num a) => [a]
coins = [5,9,13]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c:cs |c<-coins, amount>=c, cs<-change (amount - c) ]


lastElem :: [a] -> a
lastElem = foldl1 $ flip const


revRange :: (Char,Char) -> [Char]
revRange = reverse . unfoldr g 
  where g (a,b) | a > b = Nothing
                | otherwise = Just (a, (succ a, b))
                 


