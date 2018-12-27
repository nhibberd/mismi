{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
module Test.IO.Mismi.S3.Commands where

import           Control.Concurrent (threadDelay)
import           Control.Monad.Catch (catchAll, throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (runExceptT)

import "cryptohash" Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (isRight, isLeft)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Control.Lens ((^.), to)
import           Control.Monad (replicateM_) -- TODO mismi-p

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3
import qualified Mismi.S3.Amazonka as A

import           P

import qualified System.Directory as D
import           System.FilePath ((</>))
import qualified System.FilePath as F
import           System.IO (withFile, IOMode (..), hFileSize, putStrLn)
import           System.IO.Error (userError)

import           Test.Mismi.Amazonka (sendMultipart, newMultipart)
import           Test.Mismi.S3
import qualified Test.Mismi.S3.Core.Gen as Gen

import           Mismi.S3.Internal.Parallel (RunError (..))

prop_exists :: Property
prop_exists =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    lift $ writeOrFail a ""
    result <- lift $ exists a
    result === True

prop_exists_empty :: Property
prop_exists_empty =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    result <- lift $ exists a
    result === False

prop_exists_failure :: Property
prop_exists_failure =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    e <- lift $ exists a
    e === False

prop_exists_prefix :: Property
prop_exists_prefix =
  withTests 2 . property . liftAWS $ do
    k <- forAll $ Gen.genKey
    a <- newAddress
    lift $ writeOrFail (withKey (// k) a) ""
    e <- lift $ existsPrefix a
    e === True

prop_exists_prefix_missing :: Property
prop_exists_prefix_missing =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    lift $ writeOrFail a ""
    e <- lift $ existsPrefix a
    e === False

prop_exists_prefix_key :: Property
prop_exists_prefix_key =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    e <- lift $ existsPrefix a
    e === False

prop_headObject :: Property
prop_headObject =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    h <- lift $ headObject a
    h === Nothing

prop_getObjects_empty :: Property
prop_getObjects_empty =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    objs <- lift $ getObjectsRecursively $ a
    objs === []

prop_getObjectsR :: Property
prop_getObjectsR = -- d p1 p2 = p1 /= p2 ==> testAWS $ do
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.constant 5 15) Gen.alphaNum
    items <- fmap toList . forAll $ Gen.set (Range.constant 2 2) Gen.genKey
    (p1, p2) <- case items of
      p1 : p2 : [] ->
        pure (p1, p2)
      _ ->
        annotate "Invariant generator." >> failure

    root <- newAddress
    let
      keys = [p1, p2 // p1, p2 // p2]
    lift . forM_ keys $ \k ->
      writeOrFail (withKey (// k) root) d
    objs <- lift $ getObjectsRecursively root
    on (===) L.sort ((^. A.oKey . to A.toText) <$> objs) (unKey . (//) (key root) <$> keys)


-- TODO This is incredible slow
prop_pagination_list :: Property
prop_pagination_list =
  -- TODO
  withTests 0 . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 15) Gen.alphaNum
    n <- forAll $ Gen.int (Range.linear 1000 1500)
    a <- newAddress
    lift . forM_ [1..n] $ \n' ->
      writeOrFail (withKey(// Key (m <> T.pack (show n'))) a) ""
    r' <- lift $ list a
    length r' === n

prop_size :: Property
prop_size =
  withTests 10 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail a d
    i <- lift $ size a
    i === (Just . fromIntegral . BS.length $ T.encodeUtf8 d)

prop_size_failure :: Property
prop_size_failure =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    i <- lift $ size a
    i === Nothing

prop_size_recursively :: Property
prop_size_recursively =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail a d
    r <- lift $ sizeRecursively (a { key = dirname $ key a })
    r === [Sized (fromIntegral . BS.length $ T.encodeUtf8 d) a]

-- TODO This is incredible slow
prop_concat :: Property
prop_concat =
  withTests 1 . property . liftAWS $ do
    a <- newAddress
    b <- newAddress
    c <- newAddress
    f <- newFilePath
    let
      s = f </> T.unpack "fred"
      d = f </> T.unpack "down"
      bs10k = BS.concat $ L.replicate 10000 "fred"
    liftIO $ withFile s WriteMode $ \h ->
      replicateM_ 1000 (BS.hPut h bs10k)
    lift $ uploadOrFail s a
    lift $ uploadOrFail s b

    r <- lift . runExceptT $ concatMultipart Fail 1 [a, b] c
    () <- either (fail . show . renderConcatError) pure r

    lift $ downloadOrFail c d
    s' <- liftIO $ LBS.readFile s
    d' <- liftIO $ LBS.readFile d
    sha1 (LBS.concat [s', s']) === sha1 d'

prop_concat_empty_input :: Property
prop_concat_empty_input =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    r <- lift . runExceptT $ concatMultipart Fail 1 [] a
    case r of
      Left NoInputFiles ->
        success
      _ ->
        annotate "concat didn't fail correctly" >> failure

prop_concat_empty_input_files :: Property
prop_concat_empty_input_files =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    b <- newAddress
    lift $ writeOrFail a ""
    r <- lift . runExceptT $ concatMultipart Fail 1 [a] b
    case r of
      Left NoInputFilesWithData ->
        success
      _ ->
        annotate "concat didn't fail correctly" >> failure

prop_copy :: Property
prop_copy =
  withTests 2 . property . liftAWS $ do
    t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    lift $ writeOrFail a t
    lift $ either (fail . T.unpack . renderCopyError) pure =<< runExceptT (copy a b)
    a' <- lift $ read a
    b' <- lift $ read b
    a' === b'

prop_copy_missing :: Property
prop_copy_missing =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    r <- lift . runExceptT $ copy a a
    case r of
      Left (CopySourceMissing b) ->
        a === b
      _ ->
        annotate "Copy didn't fail correctly" >> failure

prop_copy_overwrite :: Property
prop_copy_overwrite =
  withTests 2 . property . liftAWS $ do
    t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    t' <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    lift $ writeOrFail a t
    lift $ writeOrFail b t'
    lift $ either (fail . T.unpack . renderCopyError) pure =<< runExceptT (copyWithMode Overwrite a b)
    b' <- lift $ read b
    b' === Just t

prop_copy_fail :: Property
prop_copy_fail =
  withTests 2 . property . liftAWS $ do
    t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    lift $ writeOrFail a t
    lift $ writeOrFail b t
    r <- lift . runExceptT $ copyWithMode Fail a b
    case r of
      Left (CopyDestinationExists z) ->
        b === z
      _ ->
        annotate "Copy didn't failure correctly" >> failure

prop_copy_multipart :: Property
prop_copy_multipart =
  withTests 1 . property . liftAWS $ do
    c <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    m <- forAll $ Gen.text (Range.linear 20 30) Gen.alphaNum
    bs <- forAll $ Gen.utf8 (Range.linear 1 100) Gen.unicodeAll

    f <- newFilePath
    a' <- newAddress
    let
      a = withKey (// Key c) a'
      b = withKey (// Key m) a'
      s = f </> T.unpack c
      d = f </> T.unpack m
    -- create large file to copy
    liftIO $ D.createDirectoryIfMissing True f
    liftIO $ withFile s WriteMode $ \h ->
      replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
    liftIO . putStrLn $ "Generated file"

    lift $ uploadOrFail s a
    liftIO . putStrLn $ "Uploaded file"

    liftIO . putStrLn $ "Running copy ..."
    lift $ either (fail . T.unpack . renderCopyError) pure =<< runExceptT (copy a b)

    liftIO . putStrLn $ "Done copy"
    -- compare
    lift $ either (fail . show) pure =<< runExceptT (download b d)
    liftIO . putStrLn $ "Done download"

    s' <- liftIO $ LBS.readFile s
    d' <- liftIO $ LBS.readFile d
    sha1 s' === sha1 d'



prop_move :: Property
prop_move =
  withTests 2 . property . liftAWS $ do
    t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    s <- newAddress
    d <- newAddress
    lift $ writeOrFail s t
    lift $ either (fail . T.unpack . renderCopyError) pure =<< runExceptT (move s d)
    es <- lift $ exists s
    ed <- lift $ exists d
    (es, ed) === (False, True)

prop_upload_mode :: Property
prop_upload_mode =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath
    m <- forAll $ Gen.genWriteMode

    p <- newFilePath
    a <- newAddress
    let
      t = p </> localPath l
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO $ T.writeFile t d
    lift $ uploadWithModeOrFail m t a
    r <- lift $ read a
    r === Just d

prop_upload_overwrite :: Property
prop_upload_overwrite =
  withTests 2 . property . liftAWS $ do
    d1 <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    d2 <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath

    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO $ T.writeFile t d1
    lift $ uploadWithModeOrFail Fail t a
    liftIO $ T.writeFile t d2
    lift $ uploadWithModeOrFail Overwrite t a
    r <- lift $ read a
    r === Just d2

prop_upload_fail :: Property
prop_upload_fail =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath

    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO $ T.writeFile t d
    lift $ uploadWithModeOrFail Fail t a
    r <- lift . runExceptT $ uploadWithMode Fail t a
    case r of
      Left (UploadDestinationExists _) ->
        success
      _ ->
        annotate "Upload succeded but should have failed" >> failure

prop_upload :: Property
prop_upload =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath
    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO $ T.writeFile t d
    lift $ uploadOrFail t a
    r <- lift $ read a
    r === Just d

prop_upload_multipart :: Property
prop_upload_multipart =
  withTests 2 . property . liftAWS $ do
    bs <- forAll $ Gen.utf8 (Range.linear 0 100) Gen.unicodeAll
    l <- forAll $ genLocalPath

    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO $ withFile t WriteMode $ \h ->
      replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
    lift $ uploadOrFail t a
    result <- lift $ exists a
    assert result

prop_abort_multipart :: Property
prop_abort_multipart =
  withTests 2 . property . liftAWS $ do

    (a, i) <- newMultipart
    lift $ sendMultipart "" a 1 i
    l <- lift $ listMultiparts (bucket a)
    let
      abortCheck :: Text -> Bucket -> Int -> A.MultipartUpload -> AWS ()
      abortCheck x b n u = do
        abortMultipart b u
        r <- listMultiparts b
        unless (n <= (0 :: Int) || L.null (findMultiparts x r)) $ do
          liftIO $ threadDelay 500000
          abortCheck x b (n-1) u

    lift . forM_ (findMultiparts i l) $
      abortCheck i (bucket a) 3

    r <- lift $ listMultiparts (bucket a)

    (L.filter (== Just i) . fmap (^. A.muUploadId) $ l) === [Just i]

    findMultiparts i r === []



prop_list_multipart :: Property
prop_list_multipart =
  withTests 2 . property . liftAWS $ do
    (a, i) <- newMultipart
    lift $ sendMultipart "" a 1 i
    l <- lift $ listMultiparts (bucket a)
    multipartExists i l

prop_list_parts :: Property
prop_list_parts =
  withTests 2 . property . liftAWS $ do
    (a, i) <- newMultipart
    lift $ sendMultipart "" a 1 i
    l2 <- lift $ listMultipartParts a i
    length l2 === 1



multipartExists :: Monad m => Text -> [A.MultipartUpload] -> PropertyT m ()
multipartExists uploadId multiparts =
  L.length (L.filter (findMultipart uploadId) multiparts) === 1

findMultiparts :: Text -> [A.MultipartUpload] -> [A.MultipartUpload]
findMultiparts uploadId =
  L.filter (findMultipart uploadId)

findMultipart :: Text -> A.MultipartUpload -> Bool
findMultipart uploadId m =
  m ^. A.muUploadId == Just uploadId


prop_list :: Property
prop_list =
  withTests 2 . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    s <- forAll $ Gen.text (Range.linear 20 30) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail (withKey (// Key m) a) ""
    lift $ writeOrFail (withKey (// (Key s // Key m)) a) ""
    r' <- lift $ list a
    (Just . Key <$> [m, s <> "/"]) === (removeCommonPrefix a <$> r')

prop_listObjects :: Property
prop_listObjects =
  withTests 2 . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    s <- forAll $ Gen.text (Range.linear 20 30) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail (withKey (// Key m) a) ""
    lift $ writeOrFail (withKey (// (Key s // Key m)) a) ""
    (p, k) <- lift $ listObjects a
    ([Just . Key $ s <> "/"], [Just $ Key m]) === (removeCommonPrefix a <$> p, removeCommonPrefix a <$> k)

prop_list_recursively :: Property
prop_list_recursively =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    lift $ writeOrFail a ""
    r' <- lift $ listRecursively (a { key = dirname $ key a })
    assert $ a `elem` r'

prop_list_forbidden_bucket :: Property
prop_list_forbidden_bucket =
  withTests 1 . property . liftAWS $ do
    _ <- lift $ write (Address (Bucket "ambiata-dev-view") (Key "")) ""
    success

prop_download :: Property
prop_download =
  withTests 1 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    l <- forAll $ genLocalPath
    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    lift $ writeOrFail a d
    r <- lift . runExceptT $ download a t
    res <- liftIO $ T.readFile t

    assert $ isRight r
    res === d

prop_download_multipart :: Property
prop_download_multipart =
  withTests 1 . property . liftAWS $ do
    c <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    m <- forAll $ Gen.text (Range.linear 20 30) Gen.alphaNum
    bs <- forAll $ Gen.utf8 (Range.linear 1 100) Gen.unicodeAll

    p <- newFilePath
    a <- newAddress
    let t = p </> T.unpack c
    let o = p </> T.unpack m
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory o
    liftIO $ withFile t WriteMode $ \h ->
      replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
    sz <- liftIO . withFile t ReadMode $ hFileSize
    lift $ uploadOrFail t a

    let ten :: Integer = 10

    r <- lift . runExceptT $ multipartDownload a o (fromInteger sz) ten 100
    b <- liftIO $ LBS.readFile t

    let b' = sha1 b
    o' <- liftIO $ LBS.readFile o
    let o'' = sha1 o'

    assert $ isRight r
    b' === o''

prop_write_download_overwrite :: Property
prop_write_download_overwrite =
  withTests 2 . property . liftAWS $ do
    old <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    new <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath
    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    lift $ writeOrFail a old
    x <- lift . runExceptT $ downloadWithMode Fail a t
    lift $ writeWithModeOrFail Overwrite a new
    y <- lift . runExceptT $ downloadWithMode Overwrite a t
    r <- liftIO $ T.readFile t

    assert $ isRight x
    assert $ isRight y
    r === new


prop_write_download_fail :: Property
prop_write_download_fail =
  withTests 2 . property . liftAWS $ do
    old <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    new <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    l <- forAll $ genLocalPath
    p <- newFilePath
    a <- newAddress
    let t = p </> localPath l
    lift $ writeOrFail a old
    x <- lift . runExceptT $ downloadWithMode Fail a t
    lift $ writeWithModeOrFail Overwrite a new
    y <- lift . runExceptT $ downloadWithMode Fail a t

    assert $ isRight x
    assert $ isLeft y

prop_delete :: Property
prop_delete =
  withTests 2 . property . liftAWS $ do
    w <- forAll Gen.genWriteMode
    a <- newAddress
    lift $ writeWithModeOrFail w a ""
    x <- lift $ exists a
    lift $ delete a
    y <- lift $ exists a
    assert x
    assert $ not y

prop_delete_empty :: Property
prop_delete_empty =
  withTests 2 . property . liftAWS $ do
    a <- newAddress
    result <- (True <$ lift (delete a)) `catchAll` (const . pure $ False)
    assert result

prop_read_write :: Property
prop_read_write =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail a d
    r <- lift $ read a
    r === Just d

prop_write_failure :: Property
prop_write_failure =
  withTests 2 . property . liftAWS $ do
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail a d
    r <- lift $ write a d
    r === WriteDestinationExists a

prop_write_overwrite :: Property
prop_write_overwrite =
  withTests 2 . property . liftAWS $ do
    let
      gen = Gen.text (Range.constant 5 15) Gen.alphaNum
    items <- fmap toList . forAll $ Gen.set (Range.constant 2 2) gen
    (x, y) <- case items of
      x : y : [] ->
        pure (x, y)
      _ ->
        annotate "Invariant generator." >> failure

    a <- newAddress
    lift $ writeWithModeOrFail Fail a x
    lift $ writeWithModeOrFail Overwrite a y
    r <- lift $ read a
    r === Just y

prop_sync_overwrite :: Property
prop_sync_overwrite =
  withTests 2 . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 5 20) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    createSmallFiles a m 10
    x <- lift . runExceptT $ syncWithMode OverwriteSync a b 1
    y <- lift . runExceptT $ syncWithMode OverwriteSync a b 1
    lift . forM_ (files b m 10) $ \e ->
      exists e >>= \e' ->
        when (e' == False) $
          (throwM . userError $ "Output files do not exist")

    assert $ isRight x
    assert $ isRight y


prop_sync_fail :: Property
prop_sync_fail =
  withTests 2 . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 5 20) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    createSmallFiles a m 1
    x <- lift . runExceptT $ syncWithMode FailSync a b 1
    y <- lift . runExceptT $ syncWithMode FailSync a b 1
    case y of
      (Left (SyncError (WorkerError (OutputExists q)))) ->
        q === withKey (// Key (m <> "-1")) b
      _ ->
        failure
    assert $ isRight x


-- | If the object does not exist, then the behaviour should be invariant with the WriteMode
prop_write_nonexisting :: Property
prop_write_nonexisting =
  withTests 2 . property . liftAWS $ do
    w <- forAll Gen.genWriteMode
    d <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeWithModeOrFail w a d
    r <- lift $ read a
    r === Just d

prop_write_grant :: Property
prop_write_grant =
  withTests 2 . property . liftAWS $ do
    t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
    a <- newAddress
    lift $ writeOrFail a t
    lift . grantReadAccess a $ ReadGrant "id=e3abd0cceaecbd471c3eaaa47bb722bf199296c5e41c9ee4222877cc91b536fc"
    r <- lift $ read a
    r === Just t

prop_read_empty :: Property
prop_read_empty =
  withTests 1 . property $ do
    k <- forAll Gen.genKey
    bucket' <- liftIO testBucket
    t <- liftIO . runAWSDefaultRegion . read $ Address bucket' k
    t === Nothing

prop_download_recursive :: Property
prop_download_recursive =
  withTests 1 . property . liftAWS $ do
    let name1 = "first name"
        name2 = "second name"
        name3 = "third name"
    tmpdir <- newFilePath
    addr <- withKey (// Key "top") <$> newAddress
    lift $ do
      writeOrFail (withKey (// Key "a") addr) name1
      writeOrFail (withKey (// Key "b/c") addr) name2
      writeOrFail (withKey (// Key "c/d/e") addr) name3

    lift $ either (fail . show) pure =<< runExceptT (downloadRecursive addr tmpdir)

    a <- liftIO $ T.readFile (tmpdir </> "a")
    c <- liftIO $ T.readFile (tmpdir </> "b" </> "c")
    e <- liftIO $ T.readFile (tmpdir </> "c" </> "d" </> "e")

    a === name1
    c === name2
    e === name3

prop_upload_recursive :: Property
prop_upload_recursive =
  withTests 1 . property . liftAWS $ do
    let name1 = "first name"
        name2 = "second name"
        name3 = "third name"
    tmpdir <- newFilePath
    liftIO $ do
      D.createDirectoryIfMissing True (tmpdir </> "b")
      D.createDirectoryIfMissing True (tmpdir </> "c" </> "d")

      T.writeFile (tmpdir </> "a") name1
      T.writeFile (tmpdir </> "b" </> "c") name2
      T.writeFile (tmpdir </> "c" </> "d" </> "e") name3

    addr <- withKey (// Key "top") <$> newAddress

    lift $ either (fail . show) pure =<< runExceptT (uploadRecursive tmpdir addr 2)

    a <- lift $ read (withKey (// Key "a") addr)
    c <- lift $ read (withKey (// Key "b/c") addr)
    e <- lift $ read (withKey (// Key "c/d/e") addr)

    a === Just name1
    c === Just name2
    e === Just name3

prop_on_status_ok :: Property
prop_on_status_ok =
  withTests 2 . property . liftAWS $ do
    let
      handler _ = Just 2
    r <- lift $ onStatus_ (1 :: Int) handler (void (exists (Address (Bucket "ambiata-dev-view") (Key ""))))
    r === 1

prop_on_status_ko :: Property
prop_on_status_ko =
  withTests 2 . property . liftAWS $ do
    let
      handler _ = Just 2
    r <- lift $ onStatus_ (1 :: Int) handler (void (write missingAddress "text"))
    r === 2

----------
-- HELPERS
----------
missingAddress :: Address
missingAddress =
  Address (Bucket "ambiata-missing") (Key "m")

sha1 :: LBS.ByteString -> Digest SHA1
sha1 =
  hashlazy

tests :: IO Bool
tests =
  checkParallel $$(discover)
