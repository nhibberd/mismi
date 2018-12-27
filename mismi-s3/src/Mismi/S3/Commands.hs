{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    headObject
  , exists
  , existsPrefix
  , getSize
  , size
  , sizeRecursively
  , delete
  , read
  , concatMultipart
  , copy
  , copyWithMode
  , copyMultipart
  , move
  , upload
  , uploadWithMode
  , uploadRecursive
  , uploadRecursiveWithMode
  , multipartUpload
  , uploadSingle
  , write
  , writeWithMode
  , getObjects
  , getObjectsRecursively
  , listObjects
  , list
  , download
  , downloadWithMode
  , downloadSingle
  , downloadWithRange
  , downloadRecursive
  , downloadRecursiveWithMode
  , multipartDownload
  , listMultipartParts
  , listMultiparts
  , listOldMultiparts
  , listOldMultiparts'
  , abortMultipart
  , abortMultipart'
  , filterOld
  , filterNDays
  , listRecursively
  , sync
  , syncWithMode
  , createMultipartUpload
  , grantReadAccess
  , chunkFilesBySize
  ) where

import           Control.Arrow ((***))
import           Control.Exception (ioError)
import qualified Control.Exception as CE
import           Control.Lens ((.~), (^.), to, view)
import           Control.Monad.Catch (Handler(..), throwM, onException)
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.Trans.Bifunctor (firstT)
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import qualified Control.Retry as Retry

import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit
import qualified Data.Conduit.List as DC

import           Data.List (filter)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (maybeToList, catMaybes, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import           Mismi.Amazonka (Env, send, paginate)
import           Mismi.Control
import           Mismi.S3.Core.Data
import           Mismi.S3.Data
import           Mismi.S3.Internal
import qualified Mismi.S3.Patch.Network as N
import qualified Mismi.S3.Patch.PutObjectACL as P
import qualified Mismi.S3.Internal.Binary as XB
import           Mismi.S3.Internal.Queue (writeQueue)
import           Mismi.S3.Internal.Parallel (consume)
import qualified Mismi.S3.Stream as Stream

import           Network.AWS.Data.Body (ChunkedBody (..), ChunkSize (..))
import           Network.AWS.Data.Body (RqBody (..), RsBody (..), toBody)
import           Network.AWS.Data.Text (toText)
import           Network.AWS.S3 (BucketName (..))
import           Network.AWS.S3 (GetObjectResponse, HeadObjectResponse)
import           Network.AWS.S3 (ListObjects)
import           Network.AWS.S3 (MetadataDirective (..))
import           Network.AWS.S3 (MultipartUpload, Part)
import           Network.AWS.S3 (Object, ObjectKey (..))
import qualified Network.AWS.S3 as A

import           P
import           Prelude (toInteger)

import           System.IO (IO, IOMode (..), SeekMode (..))
import           System.IO (hFileSize, hSetFileSize, withFile)
import           System.IO.Error (IOError)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import           System.FilePath (FilePath, (</>), takeDirectory)
import           System.Posix.IO (OpenMode(..), openFd, closeFd, fdSeek, defaultFileFlags)
import           System.Posix.Files (fileSize, getFileStatus, isDirectory, isRegularFile)
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

import qualified UnliftIO.Async as UnliftIO
import qualified UnliftIO.Timeout as UnliftIO
import           System.IO.Error (userError)

-- | Retrieves the 'HeadObjectResponse'. Handles any 404 response by converting to Maybe.
--
headObject :: Address -> AWS (Maybe HeadObjectResponse)
headObject a =
  handle404 . send . f' A.headObject $ a

-- | Checks for the existence of 'Address'.
--
exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . isJust

existsPrefix :: Address -> AWS Bool
existsPrefix (Address (Bucket b) (Key k)) =
  fmap (\r -> length (view A.lorsContents r) == 1 || length (view A.lorsCommonPrefixes r) == 1) . send $ A.listObjects (BucketName b)
    & A.loPrefix .~ Just ((+/) k)
    & A.loDelimiter .~ Just '/'
    & A.loMaxKeys .~ Just 1

getSize :: Address -> AWS (Maybe Int)
getSize a =
  size a >>= pure . fmap fromIntegral
{-# DEPRECATED getSize "Use Mismi.S3.Commands.size instead" #-}

size :: Address -> AWS (Maybe Bytes)
size a =
  headObject a >>= pure . fmap (Bytes . fromIntegral) . maybe Nothing (^. A.horsContentLength)

sizeRecursively :: Address -> AWS [Sized Address]
sizeRecursively prefix =
  runConduit $ Stream.sizeRecursively prefix .| DC.consume

-- | Delete 'Address'
--
delete :: Address -> AWS ()
delete =
  void . send . f' A.deleteObject

-- | Retrieve the object at 'Address'. Handles any 404 response by converting to Maybe.
getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' =
  handle404 . send . f' A.getObject

-- | Read contents of 'Address'.
--
read :: Address -> AWS (Maybe Text)
read a = withRetries 5 $ do
  r <- Stream.read a
  z <- liftIO . sequence $ (\x -> runResourceT . runConduit $ x .| Conduit.sinkLbs) <$> r
  pure $ fmap (T.concat . TL.toChunks . TL.decodeUtf8) z

concatMultipart :: WriteMode -> Int -> [Address] -> Address -> ExceptT ConcatError AWS ()
concatMultipart mode fork inputs dest = do
  when (mode == Fail) .
    whenM (lift $ exists dest) .
      throwE $ ConcatDestinationExists $ dest

  when (null inputs) $
    throwE NoInputFiles

  things <- fmap (join . catMaybes) . forM inputs $ \input -> do
    r <- lift $ size input
    case r of
      Nothing ->
        throwE $ ConcatSourceMissing input
      Just x ->
        let
          s = fromIntegral $ unBytes x
          minChunk = 5 * 1024 * 1024 -- 5 MiB
          chunk = 1024 * 1024 * 1024 -- 1 gb
          big = 5 * 1024 * 1024 -- 5 gb
        in
          case s == 0 of
            True ->
              pure Nothing
            False ->
              case s < minChunk of
                True ->
                  throwE $ ConcatSourceTooSmall input s
                False ->
                  case s < big of
                    True ->
                      pure $ Just [(input, 0, s)]
                    False ->
                      let
                        chunks = calculateChunksCapped s chunk 4096
                      in
                        pure . Just $ (\(a, b, _) -> (input, a, b)) <$> chunks

  when (null things) $
    throwE NoInputFilesWithData

  e <- ask
  mpu <- lift $ createMultipartUpload dest

  let
    (is, bs, ls) = L.unzip3 things
    chunks = L.zip4 is bs ls [1..]

  rs <- liftIO $
    consume (forM_ chunks . writeQueue) fork $ multipartCopyWorker e mpu dest

  let
    abort =
      lift $ abortMultipart' dest mpu

  case rs of
    Left f ->
      abort >>
        (throwE $ ConcatCopyError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload dest mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))

copy :: Address -> Address -> ExceptT CopyError AWS ()
copy s d =
  copyWithMode Overwrite s d

copyWithMode :: WriteMode -> Address -> Address -> ExceptT CopyError AWS ()
copyWithMode mode s d = do
  unlessM (lift $ exists s) . throwE $ CopySourceMissing s
  when (mode == Fail) . whenM (lift $ exists d) . throwE $ CopyDestinationExists $ d
  sz' <- lift $ getSize s
  sz <- fromMaybeM (throwE $ CopySourceSize s) sz'
  let
    chunk = 100 * 1024 * 1024 -- 100 mb
    big = 1024 * 1024 * 1024 -- 1 gb
  case sz < big of
    True ->
      lift $ copySingle s d
    False ->
      copyMultipart s d sz chunk 100

copySingle :: Address -> Address -> AWS ()
copySingle (Address (Bucket sb) (Key sk)) (Address (Bucket b) (Key dk)) =
  void . send $ A.copyObject (BucketName b) (sb <> "/" <> sk) (ObjectKey dk)
     & A.coServerSideEncryption .~ Just sse & A.coMetadataDirective .~ Just MDCopy

copyMultipart :: Address -> Address -> Int -> Int -> Int -> ExceptT CopyError AWS ()
copyMultipart source dest sz chunk fork = do
  e <- ask
  mpu <- lift $ createMultipartUpload dest -- target

  let
    chunks = calculateChunksCapped sz chunk 4096
    things = (\(o, c, i) -> (source, o, c, i)) <$> chunks

  r <- liftIO $
    consume (forM_ things . writeQueue) fork $ multipartCopyWorker e mpu dest

  let abort =
        lift $ abortMultipart' dest mpu

  case r of
    Left f ->
      abort >>
        (throwE $ MultipartCopyError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload dest mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))

-- Sort is required here because the completeMultipartUpload api expects an
-- ascending list of part id's
sortPartResponse :: [PartResponse] -> Maybe (NEL.NonEmpty A.CompletedPart)
sortPartResponse prts =
 let
   z = L.sortOn (\(PartResponse i _) -> i) prts
   l = (\(PartResponse i etag) -> A.completedPart i etag) <$> z
 in
   NEL.nonEmpty l

multipartCopyWorker :: Env -> Text -> Address -> (Address, Int, Int, Int) -> IO (Either Error PartResponse)
multipartCopyWorker e mpu dest (source, o, c, i) = do
  let
    sb = unBucket $ bucket source
    sk = unKey $ key source
    db = unBucket $ bucket dest
    dk = unKey $ key dest
    req =
      A.uploadPartCopy (BucketName db) (sb <> "/" <> sk) (ObjectKey dk) i mpu
        & A.upcCopySourceRange .~ (Just $ bytesRange o (o + c - 1))

  Retry.recovering (Retry.fullJitterBackoff 500000) [s3Condition] $ \_ -> do
    r <- runExceptT . runAWS e $ send req
    case r of
      Left z ->
        pure $! Left z

      Right z -> do
        pr <- fromMaybeM (throwM . Invariant $ "upcrsCopyPartResult") $ z ^. A.upcrsCopyPartResult
        m <- fromMaybeM (throwM . Invariant $ "cprETag") $ pr ^. A.cprETag
        pure $! Right $! PartResponse i m

createMultipartUpload :: Address -> AWS Text
createMultipartUpload a = do
  mpu <- send $ f' A.createMultipartUpload a & A.cmuServerSideEncryption .~ Just sse
  maybe (throwM . Invariant $ "MultipartUpload: missing 'UploadId'") pure (mpu ^. A.cmursUploadId)

move :: Address -> Address -> ExceptT CopyError AWS ()
move source destination' =
  copy source destination' >>
    lift (delete source)

upload :: FilePath -> Address -> ExceptT UploadError AWS ()
upload =
  uploadWithMode Fail

uploadRecursive :: FilePath -> Address -> Int -> ExceptT UploadError AWS ()
uploadRecursive =
  uploadRecursiveWithMode Fail

uploadWithMode :: WriteMode -> FilePath -> Address -> ExceptT UploadError AWS ()
uploadWithMode m f a = do
  when (m == Fail) . whenM (lift $ exists a) . throwE $ UploadDestinationExists a
  unlessM (liftIO $ doesFileExist f) . throwE $ UploadSourceMissing f
  s <- liftIO $ withFile f ReadMode $ \h ->
    hFileSize h
  case s < bigChunkSize of
    True ->
      lift $ uploadSingle f a
    False ->
      -- Originally had a concurrency of 100 (instead of 20).
      --
      -- Based on the reasoning behind downloadWithMode which resulted in a 5
      -- as it's concurrency default. Testing showed that for upload 20 was a
      -- better default.
      case s > 1024 * 1024 * 1024 of
        True ->
          multipartUpload f a s (2 * bigChunkSize) 20
        False ->
          multipartUpload f a s bigChunkSize 20



bigChunkSize :: Integer
bigChunkSize = 100 * 1024 * 1024

uploadSingle :: FilePath -> Address -> AWS ()
uploadSingle file a = do
  rq <- N.chunkedFile (ChunkSize $ 1024 * 1024) file
  void . send $ f' A.putObject a rq & A.poServerSideEncryption .~ pure sse

multipartUpload :: FilePath -> Address -> Integer -> Integer -> Int -> ExceptT UploadError AWS ()
multipartUpload file a fSize chunk fork = do
  e <- ask
  mpu <- lift $ createMultipartUpload a

  let chunks = calculateChunksCapped (fromInteger fSize) (fromInteger chunk) 4096 -- max 4096 prts returned

  r <- liftIO $
    consume (forM_ chunks . writeQueue) fork $ multipartUploadWorker e mpu file a

  let abort = lift $ abortMultipart' a mpu

  case r of
    Left f ->
      abort >>
        (throwE $ MultipartUploadError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload a mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))


multipartUploadWorker :: Env -> Text -> FilePath -> Address -> (Int, Int, Int) -> IO (Either Error PartResponse)
multipartUploadWorker e mpu file a (o, c, i) =
  withFile file ReadMode $ \h ->
    let
      cs = (1024 * 1024) -- 1 mb
      cl = toInteger c
      b = XB.slurpHandle h (toInteger o) (Just $ toInteger c)
      cb = ChunkedBody cs cl b
      req' = f' A.uploadPart a i mpu $ Chunked cb
    in
    Retry.recovering (Retry.fullJitterBackoff 500000) [s3Condition] $ \_ -> do
      r <- runExceptT . runAWS e $ send req'
      case r of
        Left z ->
          pure $! Left z
        Right z -> do
          m <- fromMaybeM (throwM MissingETag) $ z ^. A.uprsETag
          pure $! Right $! PartResponse i m

s3Condition :: Applicative a => Retry.RetryStatus -> Handler a Bool
s3Condition s =
  Handler $ \(ex :: S3Error) ->
    pure $ case ex of
      MissingETag ->
        Retry.rsIterNumber s < 5
      _ ->
        False

uploadRecursiveWithMode :: WriteMode -> FilePath -> Address -> Int -> ExceptT UploadError AWS ()
uploadRecursiveWithMode mode src (Address buck ky) fork = do
  es <- tryIO $ getFileStatus src
  case es of
    Left _ -> throwE $ UploadSourceMissing src
    Right st -> unless (isDirectory st) . throwE $ UploadSourceNotDirectory src
  files <- liftIO (listRecursivelyLocal src)
  mapM_ uploadFiles $ chunkFilesBySize fork (fromIntegral bigChunkSize) files
  where
    uploadFiles :: [(FilePath, Int64)] -> ExceptT UploadError AWS ()
    uploadFiles [] = pure ()
    uploadFiles [(f,s)]
      | fromIntegral s < bigChunkSize = lift . uploadSingle f $ uploadAddress f
      | otherwise = uploadWithMode mode f $ uploadAddress f
    uploadFiles xs =
      lift $ UnliftIO.mapConcurrently_ (\ (f, _) -> uploadSingle f $ uploadAddress f) xs


    prefixLen = L.length (src </> "a") - 1

    uploadAddress :: FilePath -> Address
    uploadAddress fp =
      Address buck (ky // Key (T.pack $ L.drop prefixLen fp))

-- Take a list of files and their sizes, and convert it to a list of tests
-- where the total size of the files in the sub list is less than `maxSize`
-- and the length of the sub lists is <= `maxCount`.
chunkFilesBySize :: Int -> Int64 -> [(FilePath, Int64)] -> [[(FilePath, Int64)]]
chunkFilesBySize maxCount maxSize =
  takeFiles 0 [] . L.sortOn snd
  where
    takeFiles :: Int64 -> [(FilePath, Int64)] -> [(FilePath, Int64)] -> [[(FilePath, Int64)]]
    takeFiles _ acc [] = [acc]
    takeFiles current acc ((x, s):xs) =
      if current + s < maxSize && L.length acc < maxCount
        then takeFiles (current + s) ((x, s):acc) xs
        else acc : takeFiles s [(x, s)] xs

-- | Like `listRecursively` but for the local filesystem.
-- Also returns
listRecursivelyLocal :: MonadIO m => FilePath -> m [(FilePath, Int64)]
listRecursivelyLocal topdir = do
  entries <- liftIO $ listDirectory topdir
  (dirs, files) <- liftIO . partitionDirsFilesWithSizes $ fmap (topdir </>) entries
  others <- concatMapM listRecursivelyLocal dirs
  pure $ files <> others


-- Not available with ghc 7.10 so copy it here.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where
    f filename =
      filename /= "." && filename /= ".."

partitionDirsFilesWithSizes :: MonadIO m => [FilePath] -> m ([FilePath], [(FilePath, Int64)])
partitionDirsFilesWithSizes =
  pworker ([], [])
  where
    pworker (dirs, files) [] = pure (dirs, files)
    pworker (dirs, files) (x:xs) = do
      xstat <- liftIO $ getFileStatus x
      let xsize = fromIntegral $ fileSize xstat
          newDirs = if isDirectory xstat then x : dirs else dirs
          newFiles = if isRegularFile xstat then (x, xsize) : files else files
      pworker (newDirs, newFiles) xs

write :: Address -> Text -> AWS WriteResult
write =
  writeWithMode Fail

writeWithMode :: WriteMode -> Address -> Text -> AWS WriteResult
writeWithMode w a t = do
  result <- runExceptT $ do
    case w of
      Fail ->
        whenM (lift $ exists a) $
          throwE (WriteDestinationExists a)
      Overwrite ->
        return ()
    void . lift . send $
      f' A.putObject a (toBody . T.encodeUtf8 $ t)
        & A.poServerSideEncryption .~ Just sse

  either pure (const $ pure WriteOk) result

-- pair of prefixs and keys
getObjects :: Address -> AWS ([Key], [Key])
getObjects (Address (Bucket buck) (Key ky)) =
  ((Key <$>) *** ((\(ObjectKey t) -> Key t) <$>)) <$> ff (A.listObjects (BucketName buck) & A.loPrefix .~ Just ((+/) ky) & A.loDelimiter .~ Just '/' )
  where
    ff :: ListObjects -> AWS ([T.Text], [ObjectKey])
    ff b = do
      r <- send b
      if r ^. A.lorsIsTruncated == Just True
        then
        do
          let d = (maybeToList =<< fmap (^. A.cpPrefix) (r ^. A.lorsCommonPrefixes), fmap (^. A.oKey) (r ^. A.lorsContents))
          n <- ff $ b & A.loMarker .~ (r ^. A.lorsNextMarker)
          pure $ d <> n
        else
        pure (maybeToList =<< fmap (^. A.cpPrefix) (r ^. A.lorsCommonPrefixes), fmap (^. A.oKey) (r ^. A.lorsContents))

getObjectsRecursively :: Address -> AWS [Object]
getObjectsRecursively (Address (Bucket b) (Key ky)) =
  getObjects' $ A.listObjects (BucketName b) & A.loPrefix .~ Just ((+/) ky)
  where
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go x ks = (NEL.toList ks <>) <$> getObjects' (x & A.loMarker .~ Just (toText $ NEL.last ks ^. A.oKey))
    getObjects' :: ListObjects -> AWS [Object]
    getObjects' x = do
      resp <- send x
      if resp ^. A.lorsIsTruncated == Just True
        then
          maybe
            (throwM . Invariant $ "Truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ resp ^. A.lorsContents)
        else
          pure $ resp ^. A.lorsContents

-- | Return a tuple of the prefixes and keys at the provided S3 Address.
listObjects :: Address -> AWS ([Address], [Address])
listObjects a =
  (\(p, k) -> (Address (bucket a) <$> p, Address (bucket a) <$> k)) <$> getObjects a

list :: Address -> AWS [Address]
list a =
  runConduit $ Stream.list a .| DC.consume

download :: Address -> FilePath -> ExceptT DownloadError AWS ()
download =
  downloadWithMode Fail

downloadWithMode :: WriteMode -> Address -> FilePath -> ExceptT DownloadError AWS ()
downloadWithMode mode a f = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist f) . throwE $ DownloadDestinationExists f
  liftIO $ createDirectoryIfMissing True (takeDirectory f)

  sz' <- lift $ getSize a
  sz <- maybe (throwE $ DownloadSourceMissing a) pure sz'

  if (sz > 200 * 1024 * 1024)
    then -- Originally had a concurrecy of 100 (instead of 5). Tested a number of
         -- values between 2 and 100 and found empirically that 5 gave the fastest
         -- downloads (less than 10% better), but significantly reduced the
         -- likelihood of triggering the S3 rate limiter (by a factor of 20)
         -- which in turn reduces the liklehood of `IOExceptions` and hung
         -- threads.
         multipartDownload a f sz 100 5
    else downloadSingle a f

downloadSingle :: Address -> FilePath -> ExceptT DownloadError AWS ()
downloadSingle a f = do
  r <- maybe (throwE $ DownloadSourceMissing a) pure =<< lift (getObject' a)
  liftIO . withRetries 5 . withFileSafe f $ \p ->
    runResourceT . runConduit $
      (r ^. A.gorsBody ^. to _streamBody) .| Conduit.sinkFile p

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> ExceptT DownloadError AWS ()
multipartDownload source destination sz chunk fork =
  firstT MultipartError $ do
    e <- ask
    let chunks = calculateChunks sz (fromInteger $ chunk * 1024 * 1024)
    void . withFileSafe destination $ \f -> do
      liftIO $ withFile f WriteMode $ \h ->
        hSetFileSize h (toInteger sz)

      ExceptT . liftIO .
        consume (\q -> mapM (writeQueue q) chunks) fork $ \(o, c, _) ->
          runExceptT . runAWS e $ downloadWithRange source o (o + c) f

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange a start end dest = withRetries 5 $ do
  -- Use a timeout of ten minutes. Arrivied at empirically. With a timeout of 5
  -- minutes this was triggering too often. Want this to be the last resort.
  res <- UnliftIO.timeout (10 * 60 * 1000 * 1000) $ do
    r <- send $ f' A.getObject a &
      A.goRange .~ (Just $ bytesRange start end)

    -- write to file
    liftIO . runResourceT $ do
      fd <- snd <$> allocate (openFd dest WriteOnly Nothing defaultFileFlags) closeFd
      void . liftIO $ fdSeek fd AbsoluteSeek (fromInteger . toInteger $ start)
      let
        source = r ^. A.gorsBody ^. to _streamBody
        sink = Conduit.awaitForever $ liftIO . UBS.fdWrite fd
      runConduit $ source .| sink

  case res of
    Just () ->
      pure ()
    Nothing ->
      liftIO $ ioError (userError "downloadWithRange timeout")

downloadRecursiveWithMode :: WriteMode -> Address -> FilePath -> ExceptT DownloadError AWS ()
downloadRecursiveWithMode mode src dest = do
  -- Check if the destination already exists and is not a directory.
  es <- tryIO $ getFileStatus dest
  case es of
    Left _ -> pure ()
    Right st -> unless (isDirectory st) . throwE $ DownloadDestinationNotDirectory dest
  -- Real business starts here.
  addrs <- lift $ listRecursively src
  mapM_ drWorker addrs
  where
    drWorker :: Address -> ExceptT DownloadError AWS ()
    drWorker addr = do
      fpdest <- maybe (throwE $ DownloadInvariant addr src) pure $
        ((</>) dest) . T.unpack . unKey <$> removeCommonPrefix src addr
      downloadWithMode mode addr fpdest

downloadRecursive :: Address -> FilePath -> ExceptT DownloadError AWS ()
downloadRecursive =
  downloadRecursiveWithMode Fail

listMultipartParts :: Address -> Text -> AWS [Part]
listMultipartParts a uploadId = do
  let req = f' A.listParts a uploadId
  runConduit $ paginate req .| DC.foldMap (^. A.lprsParts)

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts (Bucket bn) = do
  let req = A.listMultipartUploads $ BucketName bn
  runConduit $ paginate req .| DC.foldMap (^. A.lmursUploads)

listOldMultiparts :: Bucket -> AWS [MultipartUpload]
listOldMultiparts b = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterOld now) mus

listOldMultiparts' :: Bucket -> Int -> AWS [MultipartUpload]
listOldMultiparts' b i = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterNDays i now) mus

filterOld :: UTCTime -> MultipartUpload -> Bool
filterOld = filterNDays 7

filterNDays :: Int -> UTCTime -> MultipartUpload -> Bool
filterNDays n now m = case m ^. A.muInitiated of
  Nothing -> False
  Just x -> nDaysOld n now x

nDaysOld :: Int -> UTCTime -> UTCTime -> Bool
nDaysOld n now utc = do
  let n' = fromInteger $ toInteger n
  let diff = -1 * 60 * 60 * 24 * n' :: NominalDiffTime
  let boundary = addUTCTime diff now
  boundary > utc

abortMultipart :: Bucket -> MultipartUpload -> AWS ()
abortMultipart (Bucket b) mu = do
  (ObjectKey k) <- maybe (throwM $ Invariant "Multipart key missing") pure (mu ^. A.muKey)
  i <- maybe (throwM $ Invariant "Multipart uploadId missing") pure (mu ^. A.muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> Text -> AWS ()
abortMultipart' a i =
  void . send $ f' A.abortMultipartUpload a i

listRecursively :: Address -> AWS [Address]
listRecursively a =
  runConduit $ Stream.listRecursively a .| DC.consume

grantReadAccess :: Address -> ReadGrant -> AWS ()
grantReadAccess a g =
  void . send $ (f' P.putObjectACL a & P.poaGrantRead .~ Just (readGrant g))

sync :: Address -> Address -> Int -> ExceptT SyncError AWS ()
sync =
  syncWithMode FailSync

syncWithMode :: SyncMode -> Address -> Address -> Int -> ExceptT SyncError AWS ()
syncWithMode mode source dest fork = do
  e <- ask
  void . firstT SyncError . ExceptT . liftIO $
    (consume (sinkQueue e (Stream.listRecursively source)) fork (worker source dest mode e))

worker :: Address -> Address -> SyncMode -> Env -> Address -> IO (Either SyncWorkerError ())
worker input output mode env f = runExceptT . runAWST env SyncAws $ do
  n <- maybe (throwE $ SyncInvariant input f) pure $ removeCommonPrefix input f
  let out = withKey (// n) output
      liftCopy = firstT SyncCopyError
      cp = liftCopy $ copy f out
  foldSyncMode
    (ifM (lift $ exists out) (throwE $ OutputExists out) cp)
    (liftCopy $ copyWithMode Overwrite f out)
    (ifM (lift $ exists out) (pure ()) cp)
    mode

tryIO :: MonadIO m => IO a -> m (Either IOError a)
tryIO = liftIO . CE.try



-- compat

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y =
  p >>= \b -> if b then x else y

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m


fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM =
  flip maybe pure
