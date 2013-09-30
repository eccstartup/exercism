module DNA
       (
       toRNA
       ) where

import Data.String.Utils

{-
    Write a program that can translate a given DNA string to the transcribed RNA string corresponding to it.
    Both DNA and RNA strands are a sequence of nucleotides.
    The four nucleotides found in DNA are adenine (**A**), cytosine (**C**), guanine (**G**) and thymidine (**T**).
    The four nucleotides found in RNA are adenine (**A**), cytosine (**C**), guanine (**G**) and uracil (**U**).
    Given a DNA strand, its transcribed RNA strand is formed by replacing all occurrences of thymidine with uracil.
-}
toRNA :: String -> String
toRNA = replace "T" "U"
