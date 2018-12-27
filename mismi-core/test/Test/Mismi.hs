{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  , liftAWS
  , enableTests
  , runAWSDefaultRegion
  ) where

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (runExceptT)

import           Hedgehog

import           Mismi
import           Mismi.Control (unsafeRunAWS)

import           P
import           Prelude (String)

import           System.Environment (lookupEnv)
import           System.IO (IO)

testAWS :: AWS () -> Property
testAWS action =
  property $
    liftAWS (lift action)

liftAWS :: PropertyT AWS a -> PropertyT IO a
liftAWS =
  hoist runAWSDefaultRegion

-- Default to Sydney for tests only, production should fail without the environment variable
runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  r <- either (const $ pure Sydney) pure =<< runExceptT getRegionFromEnv
  e <- discoverAWSEnvWithRegion r
  unsafeRunAWS e a

-- Environment variable to lookup, tests to run when it is set to
-- false and tests to run when when it is set to true (or missing).
enableTests :: String -> [IO Bool]  -> [IO Bool] -> IO [IO Bool]
enableTests k false true = do
  d <- lookupEnv k
  pure $ bool false true $
    maybe
      False
      (\s ->
        case s of
          "true" ->
            True
          "1" ->
            True
          "false" ->
            False
          "0" ->
            False
          _ ->
            False)
      d
