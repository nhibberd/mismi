{-# LANGUAGE NoImplicitPrelude #-}
module Test.Mismi.Kernel.Gen where

import           Mismi.Kernel.Data

import           Hedgehog
import qualified Hedgehog.Gen as Gen

genMismiRegion :: Gen MismiRegion
genMismiRegion =
  Gen.enumBounded
