{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.EC2.Data (
    tests
  ) where

import           Hedgehog

import           Mismi.EC2.Data

import           P

import qualified Test.Mismi.EC2.Core.Gen as Gen


prop_instance_type :: Property
prop_instance_type =
  property $ do
    v <- forAll Gen.genMismiInstanceType
    toMismiInstanceType (fromMismiInstanceType v) === v

prop_virtualization :: Property
prop_virtualization =
  property $ do
    v <- forAll Gen.genMismiVirtualizationType
    toMismiVirtualizationType (fromMismiVirtualizationType v) === v

prop_tag :: Property
prop_tag =
  property $ do
    e <- forAll Gen.genEC2Tag
    toMismiTag (fromMismiTag e) === e

tests :: IO Bool
tests =
  checkSequential $$(discover)
