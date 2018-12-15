{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.Data where

import           Hedgehog

import           Mismi.Data

import           P

import qualified Test.Mismi.Gen as Gen

prop_tripping_region :: Property
prop_tripping_region =
  property $ do
    region <- forAll Gen.genRegion
    region === fromMismiRegion (toMismiRegion region)

tests :: IO Bool
tests =
  checkSequential $$(discover)
