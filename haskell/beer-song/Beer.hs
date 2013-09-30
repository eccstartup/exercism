module Beer
       (
         verse,
         sing
       ) where

import Data.Char (toLower)
import Data.List (intercalate)

verse :: Int -> String
verse n = lyric1 n ++ " of beer on the wall, " ++ lyric2 n ++ " of beer.\n" ++ lyric3 n ++ ", " ++ lyric2 (mod (n + 99) 100) ++ " of beer on the wall.\n"

lyric1 n
  | n == 0 = "No more bottles"
  | n == 1 = "1 bottle"
  | otherwise = show n ++ " bottles"

lyric2 = (map toLower) . lyric1

lyric3 n
  | n == 0 = "Go to the store and buy some more"
  | n == 1 = "Take it down and pass it around"
  | otherwise = "Take one down and pass it around"

sing :: Int -> Int -> String
sing n m = intercalate "\n" (map verse (reverse [m..n])) ++ "\n"

