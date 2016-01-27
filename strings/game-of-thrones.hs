import Data.List

mkRuns :: String -> [String]
mkRuns [] = []
mkRuns xs = (same : mkRuns rest) where
  (same, rest) = span (== head xs) xs 

format :: Bool -> String
format True  = "YES"
format False = "NO"

main = interact isPalindrome where
  isPalindrome xs = format $ length (odds xs) < 1
  odds xs = ((filter odd) . (map length) . mkRuns . sort) xs