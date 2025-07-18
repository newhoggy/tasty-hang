module Tasty.Hang
  ( writeStrLn
  , writeCheckImpl
  , writeCheckNamed
  , writeCheck
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Hedgehog.Internal.Config
import Hedgehog.Internal.Property
import Hedgehog.Internal.Range
import Hedgehog.Internal.Report
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Seed qualified as Seed

writeCheckImpl ::
     MonadIO m
  => (String -> IO ())
  -> UseColor
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property
  -> m (Report Result)
writeCheckImpl _ _ _ size seed prop =
  liftIO $ do
    checkReport (propertyConfig prop) size seed (propertyTest prop) $ const (pure ())

writeCheckNamed ::
     MonadIO m
  => (String -> IO ())
  -> UseColor
  -> Maybe PropertyName
  -> Maybe Seed
  -> Property
  -> m (Report Result)
writeCheckNamed write color name _ prop = do
  writeCheckImpl write color name 0 (Seed.from 1) prop

-- | Check a property.
--
writeCheck :: MonadIO m => (String -> IO ()) -> Property -> m Bool
writeCheck write prop = do
  (== OK) . reportStatus <$> writeCheckNamed write DisableColor Nothing Nothing prop

writeStrLn :: String -> IO ()
writeStrLn _ =
  pure ()
