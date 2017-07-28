module Stepic1 where

import Data.Function

-- basics

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sm, count) where
    sm = sumOfDigit x
    count = countDigit x


sumOfDigit :: Integral t => t -> t
sumOfDigit x | x' < 10 = x'
             | otherwise = (mod x' 10) + sumOfDigit (div x' 10)
  where 
    x' = abs x


countDigit :: (Enum t, Eq t, Num t, Integral t1) => t1 -> t
countDigit n = count where 
  x = abs n
  count = iter 0 x
  iter acc 0 = if acc == 0 then 1 else acc
  iter acc x' = iter (succ acc) (div x' 10)


getSecondFrom :: t2 -> t1 -> t -> t1
getSecondFrom _ y _ = y


multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on` h

h :: (a, b) -> b
h = snd

g :: Integer -> Integer -> Integer
g = (*)


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


doItYourself :: Double -> Double
doItYourself = f' . g' . h'


f' :: Double -> Double
f' = logBase 2


g' :: Double -> Double
g' = (^3)


h' :: Double -> Double
h'  = max 42 

-- Type classes

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where 
    toString (x, y) = "(" ++  toString x ++ "," ++ toString y ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab arg | (doesEnrageGork arg) && (doesEnrageMork arg) =  stomp (stab arg)
                    | doesEnrageGork arg = stab arg
                    | doesEnrageMork arg = stomp  arg
                    | otherwise = arg


class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise = pred x


avg :: Int -> Int -> Int -> Double
avg x y z = (/ 3) $ sum . map fromIntegral $ [x, y, z]


