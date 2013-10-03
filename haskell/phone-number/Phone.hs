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
prettyPrint phoneno = "(" ++ a ++ ") " ++ b ++ "-" ++ c
                    where pn = number phoneno
                          (a, bc) = splitAt 3 pn
                          (b, c) = splitAt 3 bc
