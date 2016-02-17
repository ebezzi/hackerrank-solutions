import Control.Monad
import Control.Monad.State
import Data.Map as Map
import Data.Function (fix)

type Memo = Map Int Int

memo :: Int -> Int -> State Memo ()
memo k v = do
  m <- get
  put (Map.insert k v m)

recall :: Int -> State Memo (Maybe Int)
recall k = do
  m <- get
  return (Map.lookup k m)

--openfib :: (Int -> Int) -> Int -> Int
--openfib f 0 = 0
--openfib f 1 = 1
--openfib f n = f (n-1) + f (n-2)

openfib :: (Int -> Int) -> Int -> Int
openfib f 0 = 0
openfib f 1 = 1
openfib f n = f (n-1) + f (n-2)

--type OpenFunction = Int -> 

fibo :: Int -> State Memo Int
fibo 0 = return 0
fibo 1 = return 1
fibo n = do
  prev1 <- recall (n-1)
  prev2 <- recall (n-2)
  let prev2' = case prev2 of
                  Just v -> return v
                  Nothing -> do
                    ans <- fibo (n-2)
                    memo (n-2) ans
                    return ans
  let prev1' = case prev1 of
                  Just v -> return v
                  Nothing -> do
                    ans <- fibo (n-1)
                    memo (n-1) ans
                    return ans
  prev2'' <- prev2'
  prev1'' <- prev1'
  return (prev1'' + prev2'')

fib :: Int -> Int
fib n = evalState (fibo n) Map.empty

main = do
    n <- readLn
    xs <- replicateM n readLn
    mapM_ print (map fib xs)

-- https://gist.github.com/ramntry/345139e76c3aec92e78e