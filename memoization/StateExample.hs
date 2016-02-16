import Control.Monad
import Control.Monad.State
import Data.Map

type Counter = Map Char Int

type Counter = Map Char Int

increment :: Char -> State Counter ()
increment c = do
  counter <- get
  put (insertWith (+) c 1 counter)

something :: String -> State Counter Int
something [] = return 0
something (x:xs) = do
  increment x
  next <- something xs
  return (1 + next)

main = do
  print $ runState (something "ciao a tutti da emanuele bezzi") empty