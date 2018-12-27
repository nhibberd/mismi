{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Control where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Hedgehog

import           Mismi.S3.Commands
import qualified Mismi.S3.Unsafe as Unsafe

import           P

import           Test.Mismi (liftAWS, runAWSDefaultRegion)
import           Test.Mismi.S3 (newAddressAWS)

prop_finalizer :: Property
prop_finalizer =
  withTests 2 . property . liftAWS $ do
    r <- liftIO . runAWSDefaultRegion $ do
      a <- newAddressAWS
      Unsafe.write a ""
      pure $ a

    e <- lift $ exists r

    e === False

tests :: IO Bool
tests =
  checkSequential $$(discover)
