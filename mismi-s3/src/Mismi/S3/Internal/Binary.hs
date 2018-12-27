{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.S3.Internal.Binary (
    slurpHandle
  , slurpHandleWithBuffer
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as BS
import           Data.Conduit (ConduitT, yield)

import           System.IO (Handle, hSeek, SeekMode (..))

-- This is based on `conduit-extra` version 1.1.7.3
-- Data.Conduit.Binary
--   sourceFileRange
--   souceHandleRange
--
-- Ideally this will be replaced by conduit-extra in a future release
-- based on (https://github.com/snoyberg/conduit/pull/213)
--

slurpHandle :: MonadIO m
         => Handle
         -> Integer -- ^ Offset
         -> Maybe Integer -- ^ Maximum count
         -> ConduitT () BS.ByteString m ()
slurpHandle handle offset count' =
  slurpHandleWithBuffer handle offset count' (1024 * 1024)

slurpHandleWithBuffer :: MonadIO m
                      => Handle
                      -> Integer -- ^ Offset
                      -> Maybe Integer -- ^ Maximum count
                      -> Int -- ^ Buffer size
                      -> ConduitT () BS.ByteString m ()
slurpHandleWithBuffer handle offset count' buffer = do
  liftIO $ hSeek handle AbsoluteSeek offset
  case count' of
    Nothing -> pullUnlimited
    Just c -> pullLimited (fromInteger c)
  where
    pullUnlimited = do
        bs <- liftIO $ BS.hGetSome handle buffer
        if BS.null bs
            then return ()
            else do
                yield bs
                pullUnlimited

    pullLimited c = do
        bs <- liftIO $ BS.hGetSome handle (min c $ buffer)
        let c' = c - BS.length bs
        if BS.null bs
          then return ()
          else do
            yield bs
            pullLimited c'
