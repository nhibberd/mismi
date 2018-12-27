{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Reliability.Reliability (
    testSize
  , getMaxSuccess
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Hedgehog

import           P
import qualified Prelude as P

import           System.Environment

testSize :: MonadIO m => PropertyT m Int
testSize = do
  view <- liftIO $ lookupEnv "TEST_RELIABILITY_SIZE"
  let x = maybe 10 P.read view
  pure x

getMaxSuccess :: IO Int
getMaxSuccess = do
  view <- lookupEnv "TEST_RELIABILITY_SUCCESS"
  let x = maybe 5 P.read view
  pure x
