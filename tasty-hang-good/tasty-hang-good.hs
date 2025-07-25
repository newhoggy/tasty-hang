import Prelude

import Data.ByteString qualified as BS
import Data.List qualified as L
import System.Environment qualified as E

import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $
        BS.readFile "README.md" >> pure "done"

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
