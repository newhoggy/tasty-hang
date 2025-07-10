{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude

import Control.Concurrent (getNumCapabilities)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.List qualified as L
import System.IO qualified as IO

import Hedgehog qualified as H

import UnliftIO.Async

{- HLINT ignore "Use let" -}

main :: IO ()
main = do
  numCaps <- getNumCapabilities
  IO.putStrLn $ "Using " <> show numCaps <> " capabilities for concurrent execution."
  void $
    pooledMapConcurrentlyN numCaps (void . H.check) $ L.replicate 17 $ do
      H.withTests 1 . H.withShrinks 0 . H.property $ do
        void $ H.evalIO $ BS.readFile "README.md"
