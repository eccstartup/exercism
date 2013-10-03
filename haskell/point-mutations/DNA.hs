module DNA
       (
         hammingDistance
       ) where

import Data.List (foldl')

hammingDistance :: String -> String -> Int
hammingDistance n1 n2 = foldl' (+) 0 $ zipWith diff n1 n2
                        where diff a b = fromEnum $ a /= b
