{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.S3.Core.Gen (
    genWriteMode
  , genBucket
  , genKey
  , genAddress
  , fileNameSizePairs
  ) where

import qualified Data.List as L
import qualified Data.Text as T

import           Mismi.S3.Core.Data

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


genWriteMode :: Gen WriteMode
genWriteMode =
  Gen.element [Fail, Overwrite]

genBucket :: Gen Bucket
genBucket =
  Bucket <$>
    Gen.text (Range.linear 10 20) Gen.alphaNum

-- The max length of S3 Paths is 1024 - and we append some of them in the tests
-- Unfortunately unicode characters aren't supported in the Haskell AWS library
genKey :: Gen Key
genKey =
  let
    genPath = Gen.element ["happy", "sad", ".", ":", "-"]
    listOf1 gen = Gen.list (Range.linear 1 50) gen
    path = do
      sep <- Gen.element ["-", "=", "#", ""]
      T.take 256 . T.intercalate "/" <$> listOf1 (T.intercalate sep <$> listOf1 genPath)
  in
    (Key . T.append "tests/") <$> path


genAddress :: Gen Address
genAddress =
  Gen.frequency [
      (9, Address <$> genBucket <*> genKey)
    , (1, Address <$> genBucket <*> (pure $ Key ""))
    ]

fileNameSizePairs :: Int -> Gen [(FilePath, Int64)]
fileNameSizePairs len = do
  names <- Gen.list (Range.singleton len) $ Gen.string (Range.linear 10 20) Gen.alphaNum
  lengths <- Gen.list (Range.singleton len) $ Gen.int64 (Range.linear 1 1000000000)
  let
    zipper n i l = (n <> show i, l)
  pure $ L.zipWith3 zipper names [(0::Int) ..] lengths
