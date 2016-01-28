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

main = do 
  n <- parseInt
  xs <- parseIntArray
  putStrLn $ (unwords . map show . reverse) xs