import Control.Monad

main = do
    n <- readLn
    xs <- replicateM n readLn
    mapM_ print (map fibo xs)