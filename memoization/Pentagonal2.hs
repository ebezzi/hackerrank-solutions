import Control.Monad
import Control.Monad.State
import Data.Map as Map (Map, insert, lookup, empty)
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

penta :: Int -> State Memo Int
penta 1 = return 0
penta 2 = return 5
penta n = do
  prev <- recall (n-1)
  let prev' = case prev of
                  Just v -> return v
                  Nothing -> do
                    ans <- penta (n-1)
                    memo (n-1) ans
                    return ans
  value <- prev'
  return (3 * n - 2 + value)


--fib :: Int -> (Int, Memo)
--fib n = evalState (penta n) Map.empty

--fibs :: [Int] -> State Memo [Int]
--fibs xs = mapM penta xs

--fibs :: Memo -> [Int] -> (Memo, [Int])
--fibs s []     = (s, [])
--fibs s (x:xs) = (next, v : ys) where
--  (_, ys) = fibs next xs
--  (v, next) = runState (penta x) s 

--fibs' :: [Int] -> [Int]
--fibs' xs = let (s, ws) = fibs Map.empty xs in ws

penta' :: [Int] -> State Memo [Int]
penta' xs = mapM penta xs 

main = do
    n <- readLn
    xs <- replicateM n readLn
    mapM_ print $ evalState (penta' xs) Map.empty
