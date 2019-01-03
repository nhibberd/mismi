{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.EC2.Core.Gen (
    genInstanceId
  , genLoadBalancer
  , genEC2Tag
  , genSecurityGroupName
  , genImageId
  , genEC2Market
  , genAvailabilityZone
  , genMismiSpotInstanceType
  , genMismiInstanceType
  , genMismiVirtualizationType
  ) where


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.EC2.Core.Data

import           P


genInstanceId :: Gen InstanceId
genInstanceId = do
  xs <- Gen.text (Range.linear 16 16) Gen.alphaNum
  pure $ InstanceId ("i-" <> xs)

genLoadBalancer :: Gen LoadBalancer
genLoadBalancer =
  LoadBalancer <$>
    Gen.text (Range.linear 10 20) Gen.alphaNum

genEC2Tag :: Gen EC2Tag
genEC2Tag =
  EC2Tag
    <$> Gen.text (Range.linear 5 10) Gen.alphaNum
    <*> Gen.text (Range.linear 0 20) Gen.alphaNum

genSecurityGroupName :: Gen SecurityGroupName
genSecurityGroupName =
  SecurityGroupName <$>
    Gen.text (Range.linear 5 10) Gen.alphaNum

genImageId :: Gen ImageId
genImageId = do
  xs <- Gen.text (Range.linear 8 8) Gen.alphaNum
  pure $ ImageId ("ami-" <> xs)

genEC2Market :: Gen EC2Market
genEC2Market =
  Gen.choice [
      pure EC2OnDemand
    , EC2Spot
        <$> Gen.element ["0.05", "1.10", "0.40"]
        <*> Gen.element [ OneTime, Persistent]
    ]

genAvailabilityZone :: Gen AvailabilityZone
genAvailabilityZone =
  Gen.element [
      AvailabilityZone "ap-southeast-2a"
    , AvailabilityZone "ap-southeast-2b"
    , AvailabilityZone "ap-southeast-2c"
    ]

genMismiSpotInstanceType :: Gen MismiSpotInstanceType
genMismiSpotInstanceType =
  Gen.enumBounded

genMismiInstanceType :: Gen MismiInstanceType
genMismiInstanceType =
  Gen.enumBounded

genMismiVirtualizationType :: Gen MismiVirtualizationType
genMismiVirtualizationType =
  Gen.enumBounded
