module DNA
       (
         count,
         nucleotideCounts
       ) where

import Data.Map.Strict (Map, fromList, insertWith)
import Data.List (foldl')

count :: Char -> String -> Int
count c s
  | (not $ elem c "ACTGU") = error ("invalid nucleotide " ++ show c)
  | otherwise = foldl' (+) 0 $ zipWith equ (repeat c) s
                where equ a b
                        | a == b = 1
                        | otherwise = 0

nucleotideCounts :: String -> Map Char Int
nucleotideCounts = foldl' (\map key -> insertWith (+) key 1 map) none 
                 where none = fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)]
