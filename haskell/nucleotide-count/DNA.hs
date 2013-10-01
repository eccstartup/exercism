module DNA
       (
         count,
         nucleotideCounts
       ) where

import Data.Map (Map, fromList, insertWith)
import Data.List (foldl')

count :: Char -> String -> Int
count c
  | (not $ elem c "ACTGU") = fail ("invalid nucleotide " ++ show c)
  | otherwise = length . (filter (== c))

nucleotideCounts :: String -> Map Char Int
nucleotideCounts = foldl' (\map key -> insertWith (+) key 1 map) none 
                 where none = fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)]
