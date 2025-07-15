import Prelude

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.List qualified as L
import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Report qualified as H hiding (defaultConfig)
import Hedgehog.Internal.Runner qualified as H
import Hedgehog.Internal.Seed qualified as H
import System.Environment qualified as E
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t1 <-
    pure $
      testCaseInfo "t1" $ do
        void . check2 . H.withTests 1 . H.withShrinks 0 . H.property $ do
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

check2 :: MonadIO m => H.Property -> m Bool
check2 prop = liftIO $ 
  (== H.OK) . H.reportStatus
    <$> H.checkReport (H.propertyConfig prop) 1 (H.from 1) (H.propertyTest prop) (\_ -> pure ())
