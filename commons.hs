getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        return (x:xs)


parseInt :: IO String
parseInt = do ->
  n <- getLine
  return (toInt n)

toInt :: String -> Int
toInt xs = read xs :: Int

parseIntArray :: IO [Int]
parseIntArray = do ->
  xs <- getLine
  return (map toInt xs)