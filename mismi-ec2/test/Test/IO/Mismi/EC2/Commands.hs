{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.EC2.Commands (
    tests
  ) where

import           Control.Monad.Trans.Class (lift)

import           Data.Maybe

import           Hedgehog

import           Mismi.EC2.Commands
import           Mismi.EC2.Data

import           P

import           System.IO (IO)

import           Test.Mismi


prop_findSecurityGroupByName_found :: Property
prop_findSecurityGroupByName_found =
  withTests 2 . property . liftAWS $ do
    mid <- lift $ findSecurityGroupByName (SecurityGroupName "default")
    isJust mid === True

prop_findSecurityGroupByName_not_found :: Property
prop_findSecurityGroupByName_not_found =
  withTests 2 . property . liftAWS $ do
    mid <- lift $ findSecurityGroupByName (SecurityGroupName "not-found")
    mid === Nothing


tests :: IO Bool
tests =
  checkSequential $$(discover)
