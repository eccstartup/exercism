module Bob
       (
         responseFor
       ) where

import Data.Char
import Data.List

{-
  responseFor:
    Bob is a lackadaisical teenager. In conversation, his responses are very limited.
    Bob answers 'Sure.' if you ask him a question.
    He answers 'Woah, chill out!' if you yell at him (ALL CAPS).
    He says 'Fine. Be that way!' if you address him without actually saying anything.
    He answers 'Whatever.' to anything else.
-}
responseFor :: String -> String
responseFor message
  | sayNothing message = "Fine. Be that way!"
  | allCaps message = "Woah, chill out!"
  | isQuestion message = "Sure."
  | otherwise = "Whatever."

-- | allCaps:
-- | All characters must be UPPERCASE.
-- | It must contain one or more characters in 'A-Za-z'.
allCaps :: String -> Bool
allCaps message = map toUpper message == message && any isAlpha message

-- | isQuestion:
-- | A string ends with '?' is a question.
isQuestion :: String -> Bool
isQuestion = isSuffixOf "?"

-- | sayNothing:
-- | All characters are SPACE characters.
sayNothing :: String -> Bool
sayNothing = all isSpace
