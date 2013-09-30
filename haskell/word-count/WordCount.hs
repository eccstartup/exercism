module WordCount
       (
         wordCount
       ) where

import Data.Char (isAlphaNum, toLower)
import Data.Map (Map, empty, insertWith)
import Data.List.Split (wordsBy)
import Data.List (foldl')

-- using fromListWith function is really neat.
wordCount :: String -> Map String Int
wordCount word = foldl' (\map key -> insertWith (+) key 1 map) empty $ process word

-- Format the given String
process :: String -> [String]
process word = wordsBy (not . isAlphaNum) $ map toLower word
                
                
