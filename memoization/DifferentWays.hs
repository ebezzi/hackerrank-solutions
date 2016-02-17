import Control.Monad
import Control.Monad.State
import Data.Map as Map (Map, insert, lookup, empty)
import Data.Function (fix)

type Memo k v = Map k v

memo :: (Ord a) => a -> b -> State (Memo a b) ()
memo k v = do
  m <- get
  put (Map.insert k v m)

recall :: (Ord a) => a -> State (Memo a b) (Maybe b)
recall k = do
  m <- get
  return (Map.lookup k m)

type Point = (Int, Int)

count :: Int -> Int -> State (Memo Point Int) Int
count _ 0 = return 1
count n k 
  | n == k    = return 1
  | otherwise = do
    prev1 <- recall (n-1, k-1)
    prev2 <- recall (n-1, k)
    let prev2' = case prev2 of
                    Just v -> return v
                    Nothing -> do
                      ans <- count (n-1) k
                      memo (n-1, k) ans
                      return ans
    let prev1' = case prev1 of
                    Just v -> return v
                    Nothing -> do
                      ans <- count (n-1) (k-1)
                      memo (n-1, k-1) ans
                      return ans
    prev2'' <- prev2'
    prev1'' <- prev1'
    return ((prev1'' + prev2'') `mod` (10^8+7))

countList :: [[Int]] -> State (Memo Point Int) [Int]
countList xs = mapM count' xs where
  count' ys = count (head ys) (last ys)

convert :: [String] -> [Int]
convert = map read

--              1                                   , K = 0
--count(N, K) = 1                                   , K = N
--              count(N-1, K-1) + count(N-1, K),    , 0 < K < N

main = do
  n <- readLn
  xs <- replicateM n getLine
  let ws = map ((map read) . words) xs :: [[Int]]
  mapM_ print $ evalState (countList ws) Map.empty
