import Data.Char
import Data.List

main = interact pangram where
  pangram xs 
    | length (prepare xs) == 26 = "pangram"
    | otherwise                 = "not pangram"
  prepare = nub . filter (/=' ') . map toLower 