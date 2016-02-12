{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        return (x:xs)

parseInt :: IO Int
parseInt = do
  n <- getLine
  return (toInt n)

toInt :: String -> Int
toInt xs = read xs :: Int

dropPalindrome :: Int -> String -> Bool
dropPalindrome i xs = isPalindrome ws where 
  isPalindrome ys = ys == reverse ys
  ws = (take i xs) ++ (drop (i+1) xs)

checkPalindrome :: Int -> String -> Int
checkPalindrome i xs
  | i == (length xs `div` 2) = -1
  | xs !! i == xs !! (length xs - i - 1) = checkPalindrome (i+1) xs
  | otherwise = if (dropPalindrome i xs) then i else (length xs - i -1)

--main = do
--  n <- parseInt
--  input <- getMultipleLines n
--  let res = map (checkPalindrome 0) input
--  mapM_ print res

main = do
  _ <- getLine
  xs <- getLine
  let vs = filter (\i -> xs !! i == 'a') [0..]
  print $ length vs
