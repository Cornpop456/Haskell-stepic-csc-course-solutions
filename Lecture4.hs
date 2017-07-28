module Lecture4 where 

import Data.List

import qualified Data.Monoid as M

hello :: IO ()
hello = putStrLn "Hello, lecture 4!"


testFold' :: Integer
testFold' = foldl' (*) 1 [1..100000]


newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

fn :: Endo Integer
fn = mconcat $ map Endo [(+5),(*3),(^2)]