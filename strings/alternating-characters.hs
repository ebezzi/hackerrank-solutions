
mkRuns :: String -> [String]
mkRuns [] = []
mkRuns xs = (same : mkRuns rest) where
  (same, rest) = span (== head xs) xs 

count :: String -> Int
count xs = sum $ map f (mkRuns xs) where
  f ys = length ys - 1

process :: Int -> (String -> String) -> IO ()
process 0 _ = return ()
process n f = do
  x <- getLine
  putStrLn (f x)
  process (n-1) f

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    process n (show . count)