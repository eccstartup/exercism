module Phone
       (
         areaCode,
         number,
         prettyPrint
       ) where

import Data.Char (isDigit)

number :: String -> String
number phoneno
  | length pn == 10 = pn
  | (length pn == 11) && (head pn == '1') = tail pn
  | otherwise = replicate 10 '0'
  where pn = filter isDigit phoneno

areaCode :: String -> String
areaCode phoneno = take 3 $ number phoneno

prettyPrint :: String -> String
prettyPrint phoneno = "(" ++ take 3 pn ++ ") " ++ take 3 (drop 3 pn) ++ "-" ++ drop 6 pn
                    where pn = number phoneno
