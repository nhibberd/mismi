{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.EC2.Core.Data (
    tests
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.EC2.Core.Data

import           P

import qualified Test.Mismi.EC2.Core.Gen as Gen

prop_virtualization :: Property
prop_virtualization =
  property $ do
    v <- forAll Gen.genMismiVirtualizationType
    parseVirtualization (renderVirtualization v) === Just v

prop_instance_tests :: Property
prop_instance_tests =
  property $ do
    v <- forAll Gen.genMismiInstanceType
    parseMismiInstanceType (renderMismiInstanceType v) === Just v

tests :: IO Bool
tests =
  checkSequential $$(discover)
