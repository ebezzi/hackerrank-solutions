import Data.Char

funny :: String -> Bool
funny xs = and res where
  rs = reverse xs
  n = length xs
  res = map (\x -> diff xs (fst x) (snd x) == diff rs (fst x) (snd x)) range
  diff ys i j = abs $ ord (ys !! i) - ord (ys !! j) 
  range = zip [1..(n-1)] [0..(n-2)]

--main = print $ funny "bcxz"

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
    process n printFunny where
      printFunny xs = if (funny xs) then "Funny" else "Not Funny"