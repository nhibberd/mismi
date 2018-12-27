{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Unsafe (
  -- * Upload
    hoistUploadError
  , upload
  , uploadWithMode
  , uploadRecursive
  , uploadRecursiveWithMode

  -- * Write
  , write
  , writeWithMode
  , liftWriteResult

  -- * Download
  , hoistDownloadError
  , download
  , downloadWithMode
  , downloadRecursive
  , downloadRecursiveWithMode
  ) where

import           Control.Monad.Catch (throwM)
import           Control.Monad.Trans.Except (runExceptT)

import           Mismi.Control
import qualified Mismi.S3.Commands as Commands
import           Mismi.S3.Core.Data
import           Mismi.S3.Data
import           Mismi.S3.Internal.Parallel (RunError (..))

import           P

import           System.FilePath (FilePath)


hoistUploadError :: UploadError -> AWS ()
hoistUploadError e =
  case e of
    UploadSourceMissing f ->
      throwM $ SourceFileMissing f
    UploadDestinationExists a ->
      throwM $ DestinationAlreadyExists a
    UploadSourceNotDirectory f ->
      throwM $ SourceNotDirectory f
    MultipartUploadError (WorkerError a) ->
      throwM $ a
    MultipartUploadError (BlowUpError a) ->
      throwM $ a

upload :: FilePath -> Address -> AWS ()
upload f a =
  either hoistUploadError pure =<< runExceptT (Commands.upload f a)

uploadWithMode :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithMode w f a =
  either hoistUploadError pure =<< runExceptT (Commands.uploadWithMode w f a)

uploadRecursive :: FilePath -> Address -> Int -> AWS ()
uploadRecursive f a i =
  either hoistUploadError pure =<< runExceptT (Commands.uploadRecursive f a i)

uploadRecursiveWithMode :: WriteMode -> FilePath -> Address -> Int -> AWS ()
uploadRecursiveWithMode w f a i =
  either hoistUploadError pure =<< runExceptT (Commands.uploadRecursiveWithMode w f a i)



liftWriteResult :: WriteResult -> AWS ()
liftWriteResult = \case
  WriteOk ->
    pure ()
  WriteDestinationExists a ->
    throwM $ DestinationAlreadyExists a

write :: Address -> Text -> AWS ()
write a t =
  Commands.write a t >>= liftWriteResult

writeWithMode :: WriteMode -> Address -> Text -> AWS ()
writeWithMode m a t =
  Commands.writeWithMode m a t >>= liftWriteResult


hoistDownloadError :: DownloadError -> AWS ()
hoistDownloadError e =
  case e of
    DownloadSourceMissing a ->
      throwM $ SourceMissing DownloadError a
    DownloadDestinationExists f ->
      throwM $ DestinationFileExists f
    DownloadDestinationNotDirectory f ->
      throwM $ DestinationNotDirectory f
    DownloadInvariant a b ->
      throwM $ Invariant (renderDownloadError $ DownloadInvariant a b)
    MultipartError (WorkerError a) ->
      throwM a
    MultipartError (BlowUpError a) ->
      throwM a

download :: Address -> FilePath -> AWS ()
download a f =
  either hoistDownloadError pure =<< runExceptT (Commands.download a f)

downloadWithMode :: WriteMode -> Address -> FilePath -> AWS ()
downloadWithMode m a f =
  either hoistDownloadError pure =<< runExceptT (Commands.pdownloadWithMode m a f)


downloadRecursive :: Address -> FilePath -> AWS ()
downloadRecursive a f =
  either hoistDownloadError pure =<< runExceptT (Commands.downloadRecursive a f)

downloadRecursiveWithMode :: WriteMode -> Address -> FilePath -> AWS ()
downloadRecursiveWithMode m a f =
  either hoistDownloadError pure =<< runExceptT (Commands.downloadRecursiveWithMode m a f)
