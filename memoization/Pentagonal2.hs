import Control.Monad
import Control.Monad.State
import Data.Map
import Data.Function (fix)

type Counter = Map Char Int

something :: String -> State Counter Int
something [] = return 0
something (x:xs) = do
  counter <- get
  put (insertWith (+) x 1 counter)
  next <- something xs
  return (1 + next)

main = do
  print $ runState (something "ciao a tutti da emanuele bezzi") empty

--openfib :: (Int -> Int) -> Int -> Int
--openfib f 0 = 0
--openfib f 1 = 1
--openfib f n = f (n-1) + f (n-2)

----main = print $ (fix openfib) 10

--memoize :: a -> 