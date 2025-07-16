module Main (main) where

import Lib (Countable(..))
import Control.Applicative (liftA2)

veryCostlyFunction::String->Countable Int
veryCostlyFunction s = Countable 100 (length s)

main :: IO ()
main = do
  let result = veryCostlyFunction "Hello, World!"
  print result

  -- Using liftA2 to combine two Countable values
  let anotherResult = Countable 50 10
  let combinedResult = liftA2 (+) result anotherResult
  print combinedResult

  -- Using the Monad instance of Countable
  let monadicResult = do
        x <- result
        y <- anotherResult
        return (x + y)
  print monadicResult
