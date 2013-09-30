module Anagram
       (
         anagramsFor
       ) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagramOf

isAnagramOf :: String -> String -> Bool
isAnagramOf word1 word2 = sort word1' == sort word2' && word1' /= word2'
                          where word1' = map toLower word1
                                word2' = map toLower word2

