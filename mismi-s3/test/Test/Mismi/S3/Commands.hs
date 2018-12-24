{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Commands where

import           Control.Lens ((.~))
import           Control.Monad.IO.Class (liftIO)

import qualified Data.List as DL
import           Data.Maybe (mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM
import           Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3.Commands
import qualified Mismi.S3.Amazonka as A

import           P

import qualified Test.Mismi.S3.Core.Gen as Gen

-- Positive NominalDiffTime
prop_filter_old :: Property
prop_filter_old =
  property $ do
    i <- fromInteger <$> forAll (Gen.integral $ Range.linear 1 10000)
    n <- liftIO getCurrentTime
    let
      t = addUTCTime ((-1 * ((60 * 60 * 24 * 7) + i)) :: NominalDiffTime) n
      r = filterOld n $ A.multipartUpload & A.muInitiated .~ Just t
    r === True

prop_filter_failure :: Property
prop_filter_failure =
  property $ do
    n <- liftIO getCurrentTime
    let
      r = filterOld n $ A.multipartUpload & A.muInitiated .~ Just n
    r === False

prop_chunk_files_by_size :: Property
prop_chunk_files_by_size =
  property $ do
    maxFilesPerChunk <- forAll (Gen.int $ Range.linear 2 10)
    fileCount <- forAll (Gen.int $ Range.linear 10 100)
    maxChunkSize <- forAll (Gen.int64 $ Range.linear 1000 10000)
    pairs <- forAll (Gen.fileNameSizePairs fileCount)
    let
      chunks = chunkFilesBySize maxFilesPerChunk maxChunkSize pairs
      chunkSizes = DL.map (multiChunkSum (DM.fromList pairs) . DL.map fst) chunks

    DL.filter (> maxChunkSize) chunkSizes === []

    where
      multiChunkSum :: Map FilePath Int64 -> [FilePath] -> Int64
      multiChunkSum _ [] = 0
      multiChunkSum _ [_] = 0  -- Don't care about size of single file chunk.
      multiChunkSum sizes xs =
        sum $ mapMaybe (\ x -> DM.lookup x sizes) xs

tests :: IO Bool
tests =
  checkSequential $$(discover)
