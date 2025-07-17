import Prelude

import Control.Concurrent (MVar)
import Control.Concurrent qualified as IO
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket_)
import Control.Monad
import Data.ByteString qualified as BS
import Data.List qualified as L
import Hedgehog qualified as H
import System.Environment qualified as E
import System.Exit qualified as IO
import System.IO qualified as IO
import System.IO.Unsafe qualified as IO
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T
import Test.Tasty.Runners qualified as T

{- HLINT ignore "Use let" -}

tvOuterEnter :: TVar Int
tvOuterEnter =
  IO.unsafePerformIO $ STM.newTVarIO 0
{-# NOINLINE tvOuterEnter #-}

tvOuterExit :: TVar Int
tvOuterExit =
  IO.unsafePerformIO $ STM.newTVarIO 0
{-# NOINLINE tvOuterExit #-}

tvInnerEnter :: TVar Int
tvInnerEnter =
  IO.unsafePerformIO $ STM.newTVarIO 0
{-# NOINLINE tvInnerEnter #-}

tvInnerExit :: TVar Int
tvInnerExit =
  IO.unsafePerformIO $ STM.newTVarIO 0
{-# NOINLINE tvInnerExit #-}

incCounter :: TVar Int -> IO ()
incCounter tv =
  STM.atomically $ STM.modifyTVar tv (+ 1)

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        incCounter tvOuterEnter
        void . H.check . H.withTests 1 . H.withShrinks 0 . H.property $
          void . H.evalIO $ do
            incCounter tvInnerEnter
            void $ BS.readFile "README.md"
            incCounter tvInnerExit
        incCounter tvOuterExit
          
        pure "done"

  pure $
    T.testGroup
      "test/cardano-cli-golden/cardano-cli-golden.hs"
      (L.replicate 170 t1)

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

mvLock :: MVar ()
mvLock =
  IO.unsafePerformIO $ IO.newMVar ()
{-# NOINLINE mvLock #-}

logStats :: IO ()
logStats =
  bracket_ (IO.takeMVar mvLock) (IO.putMVar mvLock ()) $ do
    (outerEnter, outerExit, innerEnter, innerExit) <- STM.atomically $ do
      outerEnter <- STM.readTVar tvOuterEnter
      outerExit <- STM.readTVar tvOuterExit
      innerEnter <- STM.readTVar tvInnerEnter
      innerExit <- STM.readTVar tvInnerExit
      pure (outerEnter, outerExit, innerEnter, innerExit)
    IO.hPutStrLn IO.stderr $ mconcat
      [ "Stats:"
      , " outer.enter=" ++ show outerEnter
      , " outer.exit=" ++ show outerExit
      , " inner.enter=" ++ show innerEnter
      , " inner.exit=" ++ show innerExit
      ]

defaultMainWithIngredients2 :: [T.Ingredient] -> T.TestTree -> IO IO.ExitCode
defaultMainWithIngredients2 ins testTree = do
  T.installSignalHandlers
  opts <- T.parseOptions ins testTree

  case T.tryIngredients ins opts testTree of
    Nothing -> do
      IO.hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      pure $ IO.ExitFailure 1
    Just act -> do
      ok <- act
      if ok then pure IO.ExitSuccess else pure $ IO.ExitFailure 1

main :: IO ()
main = do
  args <- E.getArgs
  void $ IO.forkIO $ forever $ IO.threadDelay 1000000 >> logStats
  E.withArgs ([] <> args) $ do
    ts <- tests
    exitCode <- defaultMainWithIngredients2 ingredients ts
    logStats
    IO.exitWith exitCode
