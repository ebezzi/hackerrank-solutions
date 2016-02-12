import Control.Monad
import Control.Monad.State
import Data.Map

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
