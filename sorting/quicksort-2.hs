import Control.Monad

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (p:xs) = quicksort (filter (<p) xs) ++ [p] ++ quicksort (filter (>p) xs) 

quicksortP :: [Int] -> IO [Int]
quicksortP [] = return []
quicksortP (p:xs) = do 
  lesser <- quicksortP (filter (<p) xs)
  greater <- quicksortP (filter (>p) xs)
  let ws = lesser ++ [p] ++ greater
  when (length ws > 1) (putStrLn $ format ws)
  return ws

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
  quicksortP xs