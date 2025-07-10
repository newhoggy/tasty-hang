{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude

import Control.Exception
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.List qualified as L
import System.Environment qualified as E
import System.Exit
import System.IO

import Hedgehog qualified as H
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        void . H.check . H.withTests 1 . H.withShrinks 0 . H.property $ do
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
  nStr:args <- E.getArgs
  let n = read nStr :: Int
      go i
        | i <= n = do
          catch (E.withArgs ([] <> args) $ tests >>= T.defaultMainWithIngredients ingredients) $ \exc -> print (exc :: ExitCode)
          hPutStrLn stderr $ "Successful " ++ show i ++ "/" ++ nStr ++ " iteration"
          go (i + 1 :: Int)
        | otherwise = pure ()
  go 1
