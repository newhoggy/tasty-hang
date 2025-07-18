module Tasty.Hang
  ( writeStrLn
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket_)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

mvLock :: MVar ()
mvLock = unsafePerformIO $ newMVar ()
{-# NOINLINE mvLock #-}

writeStrLn :: String -> IO ()
writeStrLn s=
  bracket_
    (takeMVar mvLock)
    (putMVar mvLock ())
    (putStrLn s)
