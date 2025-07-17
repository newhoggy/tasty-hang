import Prelude

import Control.Concurrent (MVar, newMVar)
import Control.Concurrent qualified as IO
import Control.Exception (bracket_)
import Data.List qualified as L
import System.Environment qualified as E
import System.IO.Unsafe qualified as IO
import System.Random (randomRIO)
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

withLock :: MVar () -> IO a -> IO a
withLock lock = bracket_ (IO.takeMVar lock) (IO.putMVar lock ())

lock1 :: MVar ()
lock1 = IO.unsafePerformIO $ newMVar ()
{-# NOINLINE lock1 #-}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        us1 <- randomRIO (1264848, 6609154)
        withLock lock1 $ IO.threadDelay 1000
        IO.threadDelay us1
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
