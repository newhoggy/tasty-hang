import Prelude

import Control.Concurrent qualified as IO
import Control.Monad (void)
import Data.List qualified as L
import System.Environment qualified as E
import Control.Concurrent (MVar, newMVar)

import Control.Exception (bracket_)
import Hedgehog qualified as H
import Tasty.Hang
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T
import System.Random (randomRIO)
import System.IO.Unsafe qualified as IO

withLock :: MVar () -> IO a -> IO a
withLock lock = bracket_ (IO.takeMVar lock) (IO.putMVar lock ())

theLock :: MVar ()
theLock = IO.unsafePerformIO $ newMVar ()
{-# NOINLINE theLock #-}

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        void . H.writeCheck writeStrLn . H.withTests 1 . H.withShrinks 0 . H.property $ do
          void . H.evalIO $ do
            us <- randomRIO (40282, 189064)
            withLock theLock $ IO.threadDelay 1000
            IO.threadDelay us
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
