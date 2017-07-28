module Stepic3 where

import  Data.Char

import Data.Time.Clock

import Data.Time.Format

import Data.Function

import Data.List.Split

-- Data types

charToInt :: Char -> Int
charToInt = digitToInt


data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


data Result = Fail | Success

type SomeData = String

doSomeWork :: SomeData -> (Result,Int)
doSomeWork _ = (Success, 0) 

processData :: SomeData -> String
processData d = let
    processed = doSomeWork d 
    code = snd processed 
    ans = if code == (0) then "Success" else  "Fail: " ++ (show code)
    in ans

data Result' = Fail' Int | Success'

instance Show Result' where
    show Success' = "Success"
    show (Fail' x) = "Fail: " ++ (show x)

doSomeWork' :: SomeData -> Result'
doSomeWork' d = let
    processed = doSomeWork d
    code = snd processed
    in if code /= 0 then (Fail' code) else Success'


data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Rectangle a b) = a * b

-- isSquare :: Shape -> Bool
-- isSquare  (Rectangle a b) | a == b = True
-- isSquare  _ = False

-- pattern guards
isSquare :: Shape -> Bool
isSquare rect 
    | (Rectangle a b) <- rect
    = a == b
    | _ <- rect
    = False


timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message ::  String}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString logentry = let
    time = logentry & timestamp & timeToString
    level = logentry & logLevel & show
    messag = logentry & message
    in  time ++ ": " ++  level ++ ": " ++  messag


data Coord a = Coord a a deriving Show

distance' :: Coord Double -> Coord Double -> Double
distance' (Coord x1 y1) (Coord x2 y2)  = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2)= abs (x2 - x1) + abs (y2 - y1)


getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = let 
    xCenter = helper size x
    yCenter = helper size y
    in (Coord xCenter yCenter) where
        helper size n | n < 0 = - (helper (size) (abs n))
        helper size 0 = size / 2
        helper size n = size + helper size (n - 1)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y ) = let 
    xPos = helper size x 0 (-1)
    yPos = helper size y 0 (-1)
    in (Coord xPos yPos) 
    where
        helper size coord accSize acc | coord < 0 = - (helper size (abs coord) accSize acc)
                                      | accSize > coord = acc
                                      | otherwise = helper size coord (accSize + size) (acc + 1)


-- updateLastName :: Person -> Person -> Person
-- updateLastName (Person _ ln1 _) (Person n2 _ age2) = Person n2 ln1 age2

updateLastName :: Person -> Person -> Person
updateLastName pers1 pers2 = pers2 {lastName = lastName pers1}


findDigit :: [Char] -> Maybe Char
findDigit str =  let 
    filtered =  filter isDigit str
    in if length filtered == 0 then Nothing else Just $ head filtered

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of 
    Nothing   -> 'X'
    Just char ->  char


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


-- ____________________________________________________________________________________________

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson l = if not (hasEq $ lines l)
                then Left ParsingError
                else
                    if not (hasFields l)
                    then Left IncompleteDataError
                    else
                        if not (thisDigit $ strJust $ ageString l)
                        then Left (IncorrectDataError (strJust (ageString l)))
                        else Right (Person{ firstName = strJust $ lookup "firstName" $ list2HashList l, lastName = strJust $ lookup "lastName" $ list2HashList l, age = read (strJust $ ageString l)::Int})

sumStrings acc [] = acc
sumStrings acc (x:xs) = sumStrings (acc ++ x ++" ") xs

thisDigit xs = all isDigit xs

strJust (Just x) = x
strJust (Nothing) = ""

ageString l = lookup "age" $ list2HashList l

hasFields :: String -> Bool
hasFields l = if (length (checkFields l) == 0)
              then True
              else False

checkFields l = filter (== Nothing) [lookup "firstName" $ list2HashList l, lookup "lastName" $ list2HashList l, lookup "age" $ list2HashList l]

list2HashList :: String -> [(String, String)]
list2HashList l = map l2h (lines l) where
                  l2h str = l2hM (buildPair str)
                  buildPair str = ((take (findEqN str) str), (drop ((findEqN str)+1) str))
                  l2hM p = if ((' ' == (last $ fst p)) && (' ' == (head $ snd p)))
                           then (init $ fst p, tail $ snd p)
                           else p

hasEq :: [String] -> Bool
hasEq l = foldl1 (&&) (map findEq l)


findEq :: String -> Bool
findEq [] = False
findEq (x:xs) = if (x == '=')
                then True
                else findEq xs

findEqN :: String -> Int
findEqN l = findEqN' 0 l

findEqN' :: Int -> String -> Int
findEqN' acc [] = 0
findEqN' acc (x:xs) = if (x == '=')
                     then acc
                     else findEqN' (acc+1) xs

-- ____________________________________________________________________________________________


data Coord' a = Coord' a !a

getX :: Coord' a -> a
getX (Coord' x _) = x

getY :: Coord' a -> a
getY (Coord' _ y) = y


-- рекурсивные типы данных


data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a list) = a : fromList list

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add a b = let 
    sum' = fromNat a + fromNat b
    in helper sum' where 
        helper 0 = Zero
        helper n = Suc (helper (n - 1))

mul :: Nat -> Nat -> Nat
mul a b = let 
    mul' = fromNat a * fromNat b
    in helper mul' where 
        helper 0 = Zero
        helper n = Suc (helper (n - 1))

fac :: Nat -> Nat
fac n = helper res where
        fac' 0  = 1
        fac' n'  = n' * fac' (n' - 1)
        helper 0 = Zero
        helper n'' = Suc (helper (n'' - 1))
        res = fac' (fromNat n) 


data Tree a = Leaf a | Node (Tree a) (Tree a) 

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (max (height a) (height b))

size :: Tree a -> Int
size (Leaf a) = 1
size (Node (Leaf a) (Leaf b)) = 3
size (Node a b) = 1 + size a + size b

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go tree = (leavesNum tree, leavesSum tree)

leavesNum:: Num t => Tree t1 -> t
leavesNum (Leaf _)= 1
leavesNum (Node f s) = (leavesNum f) + (leavesNum s)

leavesSum :: Num t1 => Tree t1 -> t1
leavesSum (Leaf x) = x
leavesSum (Node f s) = (leavesSum f) + (leavesSum s)

-- Синонимы и обертки для типов

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor True) (Xor False) = Xor True
    mappend (Xor False) (Xor True) = Xor True
    mappend _ _ = Xor False

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend (Maybe' Nothing) _ = Maybe' Nothing
    mappend _ (Maybe' Nothing)  = Maybe' Nothing
    mappend (Maybe' (Just x)) (Maybe' (Just y)) = Maybe' (Just (x `mappend` y))
    
