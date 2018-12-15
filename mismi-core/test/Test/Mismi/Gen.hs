{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Gen (
    genRegion
  , genAccessKey
  , genSecretKey
  , genSessionToken
  ) where

import           Network.AWS.Types

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

genRegion :: Gen Region
genRegion =
  -- Shorter list than possible, aws doesn't support all potential Regions.
  Gen.element [Ireland, Tokyo, Singapore, Sydney, NorthCalifornia, Oregon, NorthVirginia]

genAccessKey :: Gen AccessKey
genAccessKey =
  AccessKey <$>
    Gen.utf8 (Range.linear 0 50) Gen.alphaNum

genSecretKey :: Gen SecretKey
genSecretKey =
  SecretKey <$>
    Gen.utf8 (Range.linear 0 50) Gen.alphaNum

genSessionToken :: Gen SessionToken
genSessionToken =
  SessionToken <$>
    Gen.utf8 (Range.linear 0 50) Gen.alphaNum
