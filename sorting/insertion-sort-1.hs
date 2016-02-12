
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

position :: Int -> Int -> [Int] -> [Int]
position i e xs = take i xs ++ [e] ++ drop (i+1) xs

insertion :: Int -> Int -> [Int] -> [Int]
insertion i e xs 
  | (w > e)   = (position i w xs)
  | otherwise = position i e xs where
    w = xs !! i

main = do
  n <- parseInt
  xs <- parseIntArray
  let e = last xs
  --print $ format xs
  let res = insertion (length xs - 1) e xs
  print res