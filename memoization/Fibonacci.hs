import Control.Monad
import Control.Monad.State
import Data.Map as Map (Map, insert, lookup, empty)
import Data.Function (fix)

type Memo a = Map a a

memo :: (Ord a) => a -> a -> State (Memo a) ()
memo k v = do
  m <- get
  put (Map.insert k v m)

recall :: (Ord a) => a -> State (Memo a) (Maybe a)
recall k = do
  m <- get
  return (Map.lookup k m)

fibo :: Int -> State (Memo Int) Int
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
  return ((prev1'' + prev2'') `mod` (10^8+7))

fibo' :: [Int] -> State (Memo Int) [Int]
fibo' xs = mapM fibo xs 

main = do
  n <- readLn
  xs <- replicateM n readLn
  mapM_ print $ evalState (fibo' xs) Map.empty
