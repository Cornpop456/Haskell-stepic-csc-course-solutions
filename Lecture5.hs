module Lecture5 where 

import Control.Applicative

hello :: IO ()
hello = putStrLn "Hello, lecture 5!"


dist :: Applicative f => [f t] -> f [t]
dist [] = pure []
dist (ax:axs) = (:) <$> ax <*> (dist axs)

res :: ZipList [Integer]
res = dist $ map ZipList [[1,2,3]]

res2 :: ZipList [Integer]
res2 = ( (:) <$> head (map ZipList [[1,2,3]])) <*> (pure [])


f :: Show a => a -> [String]
f x = [show x]

res3 :: Show a => a -> [Maybe String]
res3 x = traverse f (Just x)

res4 :: Show a => a -> Maybe [String]
res4 x = sequenceA $ res3 x