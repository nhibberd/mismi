{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Internal where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3.Internal

import           P

prop_chunks :: Property
prop_chunks =
  property $ do
    size <- forAll $ Gen.int (Range.linear 1 10000)
    chunk <- forAll $ Gen.int (Range.linear 1 size)
    foldl' (+) 0 (snd' <$> calculateChunks size chunk) === size

prop_chunks_capped :: Property
prop_chunks_capped =
  property $ do
    size <- forAll $ Gen.int (Range.linear 10 10000)
    chunk <- forAll $ Gen.int (Range.linear 1 size)
    cap <- forAll $ Gen.int (Range.constant 5 15)
    assert $ length (calculateChunksCapped size chunk cap) <= cap

snd' :: (Int, Int, Int) -> Int
snd' (_, b, _) = b

tests :: IO Bool
tests =
  checkSequential $$(discover)
