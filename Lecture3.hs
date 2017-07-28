module Lecture3 where

hello :: IO ()
hello = putStrLn "Hello, lecture 3!"


data Tree a = Leaf a | Branch (Tree a) a (Tree a)

instance (Eq a) => Eq (Tree a) where
    Leaf x == Leaf y  = x == y
    (Branch f1 d1 s1) == (Branch f2 d2 s2) = d1 == d2 && f1 == f2 && s1 == s2

elemTree :: Eq t => t -> Tree t -> Bool
elemTree e (Leaf x) = e == x
elemTree e (Branch f d s) = e == d || elemTree e f || elemTree e s

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch one el two) = Branch (fmap f one ) (f el) (fmap f two)


data List a = Nil | Cons a (List a) deriving Show

-- instance Show a => Show (List a) where
--     showsPrec _ = myShowsList
    
-- myShowsList :: Show a => List a -> ShowS
-- myShowsList Nil = showString "|"
-- myShowsList (Cons x xs) = showChar '<'
--     . shows x 
--     . myShowsList xs
--     . showChar '>'

-- instance Show a => Show (List a) where
--     show = myShowList 

-- myShowList :: Show a => List a -> String
-- myShowList  Nil         = "|"
-- myShowList  (Cons x xs) = "<"
--                         ++ show x
--                         ++ myShowList xs
--                         ++ ">"

instance Show a => Show (Tree a) where
    show = myShowsTree
    
myShowsTree :: Show a => Tree a -> String
myShowsTree (Leaf x) =  show x
myShowsTree (Branch f d s) = "<" 
                            ++ myShowsTree f 
                            ++ "{" ++ show d ++ "}"
                            ++  myShowsTree s
                            ++ ">"


myReadsList :: (Read a) => ReadS (List a)
myReadsList ('|':s) = [(Nil, s)]
myReadsList ('<':s) = [(Cons x l, u) | (x, t)     <- reads s,
                                       (l, '>':u) <- myReadsList t ]


newtype Matrix a = Matrix [[a]]

instance (Show a, Eq a) => Show (Matrix a) where
    show = myShowMatrix 

myShowMatrix  :: (Show a, Eq a) => Matrix a  -> String
myShowMatrix (Matrix []) = "EMPTY"
myShowMatrix (Matrix (x:xs)) | xs == [] = showList x ""
                             | otherwise = (showList x . showChar '\n') "" ++ myShowMatrix (Matrix xs)


rotate :: Int -> [a] -> [a]
rotate n l | n ==  (length l) || n == 0 = l
           | n > 0 = 
    let firstElems = drop n l
        lastElems  = take n l
    in firstElems ++ lastElems
           | n < 0 = 
    let firstElems = drop ((length l) + n) l
        lastElems  = take ((length l) + n) l
    in firstElems ++ lastElems


comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]  -- only the empty list has 0 elements
comb _ []     = []    -- can not extract > 0 elements from []
comb i (x:xs) = [ x:ys | ys <- comb (i-1) xs ]  -- keep x case
                ++ comb i xs                    -- discard x case
