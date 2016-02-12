{-# LANGUAGE ViewPatterns #-}
import Data.Char

incrementChar :: Char -> Char
incrementChar 'z' = 'a'
incrementChar c   = chr (1 + ord c)

increment :: String -> String
increment (x:xs)  
  | x == 'z'  = 'a' : increment xs
  | otherwise = incrementChar x : xs

shift :: String -> String
shift (x:xs) = xs

main = print $ dshift "ciao a tutti" where 
  dshift = reverse . shift . reverse
