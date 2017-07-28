module Lecture2 where

hello :: IO ()
hello = putStrLn "Hello, lecture 2!"


factorial :: (Ord t, Num t) => t -> t
factorial n = helper 1 n
  where helper acc k | k > 1 = (helper $! acc * k) (k - 1)
                     | otherwise = acc


data List a = Nil | Cons a (List a) deriving Show

len :: Num t => List t1 -> t

len Nil = 0

len (Cons _ xs) = 1 + len xs


countEven :: (Num t, Integral a) => [a] -> t
countEven l  | l == []  = error "EMPTY!!!"
             | otherwise =  counter 0 l where 
  counter acc (y:ys)   | (ys == []) && (rem y 2 == 0) = acc + 1 
                       | (ys == []) && (rem y 2 /= 0) = acc
                       | rem y 2 == 0 = counter (acc + 1) ys
                       | otherwise = counter acc ys


getOdds :: Integral a => [a] -> [a]
getOdds l  | l == []  = error "EMPTY!!!"
           | otherwise =  helper [] l where 
  helper acc (y:ys)    | (ys == []) && (rem y 2 == 0) = acc
                       | (ys == []) && (rem y 2 /= 0) = y : acc
                       | rem y 2 /= 0 = helper (y : acc) ys
                       | otherwise = helper acc ys


countPairs :: (Num a, Eq a) => [a] -> [a] -> [a]
countPairs l1 l2 | l1 == [] && l2 == [] = []
                 | l1 == [] && l2 /= [] = l2
                 | l1 /= [] && l2 == [] = l1
                 | otherwise = helper [] l1 l2 where 
  helper acc (x:xs) (y:ys) | xs == [] || ys == [] = acc ++ [x + y]
                           | otherwise = helper (acc ++ [x + y]) xs ys


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp  Error _ = GT
cmp  _ Error = LT
cmp Warning Warning = EQ
cmp Warning Info = GT
cmp Info Info = EQ
cmp Info Warning = LT


data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = f, lastName = l, age = a})  
    | length f < 2 = p
    | otherwise = Person {firstName=(head f : "."), lastName=l, age=a}


data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

treeTest :: Tree Integer
treeTest = Node (Node Leaf 5 (Node Leaf 4 Leaf)) 10 (Node Leaf 20 (Node Leaf 50 (Node Leaf 100 Leaf)))

treeSum :: Tree Integer -> Integer
treeSum Leaf = error "Empty tree!"
treeSum tree = helper 0 tree where 
    helper acc (Node f d s) | (f == Leaf && s == Leaf) = acc + d
                            | (f /= Leaf && s /= Leaf) = (helper (acc + d) f) + (helper acc s)
                            | (f/=Leaf) = helper (acc + d) f
                            | (s /= Leaf) = helper (acc + d) s

treeHeight :: (Eq t1, Ord t, Num t) => Tree t1 -> t
treeHeight Leaf = 0
treeHeight (Node Leaf _ Leaf) = 1
treeHeight tree = helper 0 tree where 
    helper acc (Node f _ s) | (f /= Leaf && s /= Leaf) = 
            
            let {fHeight = (helper (acc + 1) f); sHeight = (helper (acc + 1) s)}                                        
            in  if fHeight >=  sHeight then fHeight else sHeight                                                                   
                            
                            | f /= Leaf = helper (acc + 1) f
                            | s /= Leaf = helper (acc + 1) s
                            | otherwise = acc + 1


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 l1 [] [] = l1
sum3 [] l2 [] = l2
sum3 [] [] l3 = l3
sum3 l1 l2 l3 = helper l1 l2 l3 [] where 
    helper [] [] [] acc = acc
    helper (x:xs) [] [] acc = helper xs [] [] (acc ++ [x])
    helper [] (y:ys) [] acc = helper [] ys [] (acc ++ [y])
    helper [] [] (z:zs) acc = helper [] [] zs (acc ++ [z])
    helper (x:xs) (y:ys) [] acc = helper xs ys [] (acc ++ [x + y])
    helper (x:xs) [] (z:zs) acc = helper xs [] zs (acc ++ [x + z])
    helper [] (y:ys) (z:zs) acc = helper [] ys zs (acc ++ [y + z])
    helper (x:xs) (y:ys) (z:zs) acc = helper xs ys zs (acc ++ [x + y + z])



-- digits :: Integral a => a -> [a]
-- digits n | n < 10 = [n]
--          | otherwise =  digits (div n 10) ++ [rem n 10]

digits :: Integer -> [Integer]
digits n = map (\x -> read [x]::Integer) (show n)

containsAllDigits :: Integer -> Bool
containsAllDigits n = let 
    dig = [1,2,3,4,5,6,7,8,9]
    digOfN = digits n
    check [] _ = True
    check (x:xs) dN | (elem x dN == True) = check xs dN
                    | otherwise = False
    in check dig digOfN