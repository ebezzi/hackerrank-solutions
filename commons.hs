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

parseIntArray :: IO [Int]
parseIntArray = do
  xs <- getLine
  return $ map toInt (words xs)

format :: [Int] -> String
format xs = unwords (map show xs)




-- Strings

incrementChar :: Char -> Char
incrementChar 'z' = 'a'
incrementChar c   = chr (1 + ord c)

increment :: String -> String
increment (x:xs)  
  | x == 'z'  = 'a' : increment xs
  | otherwise = incrementChar x : xs

