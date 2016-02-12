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

main = do
  n <- parseInt
  xs <- parseIntArray
  let p = head xs
  let ws = filter (<p) xs ++ [p] ++ filter (>p) xs
  putStrLn (format ws)
