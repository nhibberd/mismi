{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.Kernel.Data (tests) where

import           Hedgehog

import           Mismi.Kernel.Data

import           P

import qualified Test.Mismi.Kernel.Gen as Gen

prop_region :: Property
prop_region =
  property $ do
    region <- forAll Gen.genMismiRegion
    let
      result = parseMismiRegion $ renderMismiRegion region
    result === Just region

tests :: IO Bool
tests =
  checkSequential $$(discover)
