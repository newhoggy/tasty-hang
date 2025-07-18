import Prelude

import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.List qualified as L
import System.Environment qualified as E

import Hedgehog qualified as H
import System.CPUTime
import Tasty.Hang
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T
import Text.Printf

{- HLINT ignore "Use let" -}

timeIt :: IO a -> IO a
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = end - start
  printf "Computation time: %d picosecs\n" (diff :: Integer)
  return result

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        timeIt . void . H.writeCheck writeStrLn . H.withTests 1 . H.withShrinks 0 . H.property $ do
          void . H.evalIO $ BS.readFile "README.md"
        pure "done"

  pure $
    T.testGroup
      "test/cardano-cli-golden/cardano-cli-golden.hs"
      (L.replicate 170 t1)

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] <> args) $ tests >>= T.defaultMainWithIngredients ingredients
