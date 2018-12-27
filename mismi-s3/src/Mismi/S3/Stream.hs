{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Stream (
    sizeRecursively
  , read
  , list
  , liftAddressAndPrefix
  , listRecursively
  , liftAddress
  ) where

import qualified Control.Exception as CE
import           Control.Lens ((.~), (^.), to)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Retry as Retry

import qualified Data.ByteString as BS
import           Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as DC
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Maybe (maybeToList)

import           Mismi.Amazonka (Env, send, paginate)
import           Mismi.Control (AWS, throwOrRetry, handle404)
import           Mismi.S3.Core.Data
import           Mismi.S3.Internal (f', (+/), bytesRange)

import qualified Network.AWS as A
import           Network.AWS.Data.Body (RsBody (..))
import           Network.AWS.S3 (BucketName (..))
import           Network.AWS.S3 (ListObjectsResponse)
import           Network.AWS.S3 (ObjectKey (..))
import qualified Network.AWS.S3 as A

import           P


takeObjectSizes :: Bucket -> ListObjectsResponse -> [Sized Address]
takeObjectSizes b lors =
  with (lors ^. A.lorsContents) $ \o ->
    let
      ObjectKey k =
        o ^. A.oKey

      bytes =
        Bytes $ fromIntegral (o ^. A.oSize)
        -- We shouldn't need this fromIntegral but amazonka incorrectly uses
        -- an Int instead of Int64 for sizes, we don't want to propagate this
        -- mistake.
        --
        -- See https://github.com/brendanhay/amazonka/issues/320
    in
      Sized bytes $ Address b (Key k)

sizeRecursively :: Address -> ConduitT () (Sized Address) AWS ()
sizeRecursively (Address b (Key k)) =
  let
    cmd =
      A.listObjects (BucketName $ unBucket b)
        & A.loPrefix .~ Just k
  in
    paginate cmd .|
      DC.mapFoldable (takeObjectSizes b)

countBytes ::
      IORef Int64
   -> ConduitT () BS.ByteString (ResourceT IO) ()
   -> ConduitT () BS.ByteString (ResourceT IO) ()
countBytes ref src =
  let
    loop = do
      mbs <- Conduit.await
      case mbs of
        Nothing ->
          pure ()
        Just bs -> do
          liftIO $ IORef.modifyIORef' ref (+ fromIntegral (BS.length bs))
          Conduit.yield bs
          loop
  in
    src .| loop

readRange ::
     Int
  -> Int
  -> Address
--  -> AWS (ConduitT BS.ByteString (ResourceT IO) (), ResourceT IO ())
  -> AWS (ConduitT () BS.ByteString (ResourceT IO) ())
readRange start end a = do
  result <-
    send $
      f' A.getObject a
        & A.goRange .~ Just (bytesRange start end)

--  liftResourceT $
  pure $
    result ^. A.gorsBody . to _streamBody

readRetry ::
     Env
  -> Retry.RetryStatus
--  -> IORef (ResourceT IO ())
  -> IORef Int64
  -> Int
  -> Address
  -> ConduitT () BS.ByteString (ResourceT IO) ()
readRetry env status0 startRef end a = do
  start <- fmap fromIntegral . liftIO $ IORef.readIORef startRef

  source <- A.runAWS env $ readRange start end a
--  liftIO $ IORef.writeIORef finalizerRef finalizer

  Conduit.catchC (countBytes startRef source) $ \(err :: CE.SomeException) -> do
    status <- liftIO $ throwOrRetry 5 err status0
    readRetry env status startRef end a

--newResumableSource :: Source m a -> m () -> ResumableSource m a
--newResumableSource (Conduit.ConduitM source) final =
--  Conduit.ResumableSource (source Conduit.Done) final


-- | WARNING : The returned @ResumableResource@ must be consumed within the
-- @AWS@ monad. Failure to do so can result in run time errors (recv on a bad
-- file descriptor) when the @MonadResouce@ cleans up the socket.
read :: Address -> AWS (Maybe (ConduitT () BS.ByteString (ResourceT IO) ()))
read a = do
  env <- ask
  startRef <- liftIO $ IORef.newIORef 0
  mend <-
    (handle404 . send . f' A.headObject $ a) >>= pure . fmap (Bytes . fromIntegral) . maybe Nothing (^. A.horsContentLength) >>= pure . fmap fromIntegral

  case mend of
    Nothing ->
      pure Nothing

    Just 0 ->
      pure $ Just mempty
--      pure . Just $ newResumableSource mempty (pure ())

    Just end -> do
--      finalizerRef <- liftIO $ IORef.newIORef (pure ())

      let
        source =
          readRetry env Retry.defaultRetryStatus startRef end a

--        final = do
--          finalizer <- liftIO $ IORef.readIORef finalizerRef
--          finalizer

      pure . Just $ source
--        newResumableSource source final

list :: Address -> ConduitT () Address AWS ()
list a@(Address (Bucket b) (Key k)) =
  let
    run s = s .| liftAddressAndPrefix a
  in
    run . paginate $
      A.listObjects (BucketName b)
        & A.loPrefix .~ Just ((+/) k)
        & A.loDelimiter .~ Just '/'

liftAddressAndPrefix :: Address -> ConduitT ListObjectsResponse Address AWS ()
liftAddressAndPrefix a =
  DC.mapFoldable (\r ->
       fmap (\o ->
         let ObjectKey t = o ^. A.oKey
         in a { key = Key t }) (r ^. A.lorsContents)
    <> join (forM (r ^. A.lorsCommonPrefixes) $ \cp ->
         maybeToList . fmap (\cp' -> a { key = Key cp' }) $ cp ^. A.cpPrefix))

listRecursively :: Address -> ConduitT () Address AWS ()
listRecursively a@(Address (Bucket bn) (Key k)) =
  paginate (A.listObjects (BucketName bn) & A.loPrefix .~ Just k) .| liftAddress a

liftAddress :: Address -> ConduitT ListObjectsResponse Address AWS ()
liftAddress a =
  DC.mapFoldable (\r -> (\o -> a { key = Key (let ObjectKey t = o ^. A.oKey in t) }) <$> (r ^. A.lorsContents) )
