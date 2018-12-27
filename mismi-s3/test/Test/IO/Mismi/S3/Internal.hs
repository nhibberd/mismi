{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Internal where

import           Control.Monad.Catch (catchIOError)
import           Control.Monad.IO.Class (liftIO)

import           Data.Text.IO as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3.Internal

import           P

import           System.Directory (doesFileExist)
import           System.IO.Temp (withSystemTempDirectory)

prop_withFileSafe :: Property
prop_withFileSafe =
  property $ do
    t <- forAll $ Gen.text (Range.constant 5 15) Gen.alphaNum
    (f2e, t2) <- liftIO . withSystemTempPath $ \f -> do
      f2 <- withFileSafe f $ \f2 -> do
        T.writeFile f2 t
        pure f2
      t2 <- T.readFile f
      f2e <- doesFileExist f2
      pure (f2e, t2)
    (t, f2e) === (t2, False)


prop_withFileSafe_empty :: Property
prop_withFileSafe_empty =
  property $ do
  f2 <- liftIO . withSystemTempPath $ \f ->
    flip catchIOError (\_ -> pure Nothing) $ withFileSafe f (pure . Just)
  f2 === Nothing

prop_withFileSafe_error :: Property
prop_withFileSafe_error =
  property $ do
    result <- liftIO . withSystemTempPath $ \f -> do
      flip catchIOError (\_ -> pure ()) . withFileSafe f $ \_ -> fail ""
      fmap not $ doesFileExist f
    assert result

prop_withFileSafe_error_exists :: Property
prop_withFileSafe_error_exists =
  property $ do
    t <- forAll $ Gen.text (Range.constant 5 15) Gen.alphaNum
    result <- liftIO . withSystemTempPath $ \f -> do
      flip catchIOError (\_ -> pure ()) . withFileSafe f $ \f2 -> T.writeFile f2 t >> fail ""
      fmap not $ doesFileExist f
    assert result

withSystemTempPath :: (FilePath -> IO t) -> IO t
withSystemTempPath run =
  withSystemTempDirectory "mismi-s3-file-safe" $ \dir ->
    run $ dir <> "/file"


tests :: IO Bool
tests =
  checkSequential $$(discover)
-- TODO
--tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 20 })
