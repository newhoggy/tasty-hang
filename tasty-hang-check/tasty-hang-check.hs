module Main where

import Data.ByteString qualified as BS
import HaskellWorks.Prelude
import Hedgehog qualified as H
import System.Exit
import Test.Check.Core

{- HLINT ignore "Use let" -}

main :: IO ()
main = do
  tests <- forM (id @[Int] [0..170 - 1]) $ \i -> do
    pure $
      Test
      { name = TestName $ "tasty-hang-veritas-" <> show i
      , description = "Test " <> show i
      , test = id @(IO Bool) $
          H.check $ H.withTests 1 . H.withShrinks 0 . H.property $ do
            void $ H.evalIO $ BS.readFile "README.md"
      }

  plan <- mkPlan tests

  runPlan plan

  reportResults plan

  checkResults plan >>= exitWith
