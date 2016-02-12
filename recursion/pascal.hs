
fac n = foldl (*) 1 [1..n]

pascal :: Int -> [[Int]]
pascal 0 = [[1]]
pascal n = pascal (n-1) ++ [map comb [0..n]] where 
   comb r = (fac n) `div` (fac r * fac (n-r)) -- n! / (r! * (n-r)!)


format :: [Int] -> String
format xs = unwords (map show xs)

main = do 
  n <- readLn
  mapM_ (putStrLn . format) (pascal (n-1))