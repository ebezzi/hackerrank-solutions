import Control.Monad.State
import qualified Data.Map as Map

recall :: Ord a => a -> State (Map.Map a r) (Maybe r)
recall n = do
  memory <- get
  return (Map.lookup n memory)

memorize :: Ord a => a -> r -> State (Map.Map a r) ()
memorize n result = do
  memory <- get
  put (Map.insert n result memory)

type MemorizedResult a r = State (Map.Map a r) r
type ClosedFunction a r = a -> MemorizedResult a r
type OpenFunction a r = ClosedFunction a r -> ClosedFunction a r

memoized :: Ord a => OpenFunction a r -> a -> r
memoized client arg = evalState (closeThroughMemory client arg) Map.empty

closeThroughMemory :: Ord a => OpenFunction a r -> ClosedFunction a r
closeThroughMemory client n = do
  recalled <- recall n
  case recalled of
    Just result -> return result
    Nothing -> do
      result <- client (closeThroughMemory client) n
      memorize n result
      return result


openFibonacci :: OpenFunction Int Integer
openFibonacci self n = do
  if n < 2
  then return (fromIntegral n)
  else do
    prev1 <- self (n - 1)
    let result = prev1 + 3 * fromIntegral n - 2
    return result

fibonacci :: Int -> Integer
fibonacci = memoized openFibonacci


main :: IO ()
main = do
  --putStrLn "n = "
  --n <- getLine
  --let result = fibonacci (read n)
  --putStrLn $ "fibonacci(" ++ n ++ ") = " ++ show result
  n <- readLn
  xs <- replicateM n readLn
  mapM_ print (map fibonacci xs)