{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Core.Data (
    WriteMode (..)
  , SyncMode (..)
  , Bucket (..)
  , Address (..)
  , Key (..)
  , ReadGrant (..)
  , WriteResult (..)
  , Bytes (..)
  , Sized (..)
  , (//)
  , combineKey
  , dirname
  , foldWriteMode
  , foldSyncMode
  , basename
  , addressFromText
  , addressToText
  , removeCommonPrefix
  , withKey
  , s3Parser
  ) where

import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text (anyChar, char, manyTill, string, takeWhile)
import           Data.Attoparsec.Text (endOfInput, parseOnly)
import           Data.Data (Data, Typeable)
import           Data.List (drop, init, reverse, zipWith)
import           Data.String (String)
import qualified Data.Text as T

import           P
import           Prelude (Integral)

data WriteResult =
    WriteOk
  | WriteDestinationExists !Address
    deriving (Eq, Show)

-- |
-- Describes the semantics for destructive operation that may result in overwritten files.
--
data WriteMode =
    Fail        -- ^ Fail rather than overwrite any data.
  | Overwrite   -- ^ Overwrite existing data silently, i.e. we really want to do this.
    deriving (Eq, Show)

foldWriteMode :: a -> a -> WriteMode -> a
foldWriteMode f o m =
  case m of
    Fail ->
      f
    Overwrite ->
      o

data SyncMode =
    FailSync
  | OverwriteSync
  | SkipSync
    deriving (Eq, Show)

foldSyncMode :: a -> a -> a -> SyncMode -> a
foldSyncMode f o s m =
  case m of
    FailSync ->
      f
    OverwriteSync ->
      o
    SkipSync ->
      s

newtype Bucket =
  Bucket {
      unBucket :: Text
    } deriving (Eq, Show, Ord, Data, Typeable)

newtype Key =
  Key {
      unKey :: Text
    } deriving (Eq, Show, Ord, Data, Typeable)

data Address =
  Address {
      bucket :: !Bucket
    , key :: !Key
    } deriving (Eq, Show, Ord, Data, Typeable)

newtype ReadGrant =
  ReadGrant {
      readGrant :: Text
    } deriving (Eq, Show)

newtype Bytes =
  Bytes {
      unBytes :: Int64
    } deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

data Sized a =
  Sized {
      sizedBytes :: !Bytes
    , sizedValue :: !a
    } deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

(//) :: Key -> Key -> Key
(//) =
  combineKey

combineKey :: Key -> Key -> Key
combineKey (Key p1) (Key p2) =
  if  "/" `T.isSuffixOf` p1 || p1 == "" || "/" `T.isPrefixOf` p2 then
    Key $ p1 <> p2
  else
    Key $ p1 <> "/" <> p2

-- | @withKey f address@ : Replace the 'Key' part of an 'Address' with a new
--   'Key' resulting from the application of function @f@ to the old 'Key'.
withKey :: (Key -> Key) -> Address -> Address
withKey f (Address b k) =
  Address b $ f k

-- | Get the prefix for a given key (eg. dirname "\/foo\/bar" == "foo").
dirname :: Key -> Key
dirname =
  Key . T.intercalate "/" . init . T.split (=='/') . unKey

-- | Get the basename for a given key (eg. basename "\/foo\/bar" == "bar").
--   Return 'Nothing' for the empty 'Key' _and_ when the name ends with a "/".
basename :: Key -> Maybe Text
basename =
  mfilter (not . T.null) . listToMaybe . reverse . T.split (== '/') . unKey

-- prefix key
removeCommonPrefix :: Address -> Address -> Maybe Key
removeCommonPrefix prefix addr =
  let
    dropMaybe :: String -> String -> Maybe Text
    dropMaybe x y =
      bool
        Nothing
        (Just . T.pack $ drop (length y) x)
        (check x y)

    check :: String -> String -> Bool
    check x y =
      y == zipWith const x y
  in
  if bucket addr == bucket prefix then
     if unKey (key prefix) == "" then
        Just $ key addr
     else
       let
         bk = unKey (key prefix)
         b = bool (bk <> "/") bk ("/" `T.isSuffixOf` bk)
         pk = T.unpack b
         kk = T.unpack (unKey $ key addr)
       in
         Key <$> dropMaybe kk pk
  else
    Nothing

-- | Render an 'Address' to 'Text', including the "s3://" prefix.
addressToText :: Address -> Text
addressToText a =
  "s3://" <> unBucket (bucket a) <> "/" <> unKey (key a)

-- | Parse an 'Address' from 'Text'. If the parse fails, 'Nothing' is returned.
addressFromText :: Text -> Maybe Address
addressFromText =
  hush . parseOnly s3Parser

s3Parser :: Parser Address
s3Parser =
  s3Parser' <|> s3Parser''

s3Parser' :: Parser Address
s3Parser' = do
  _ <- string "s3://"
  b <- manyTill anyChar (char '/')
  k <- many anyChar
  pure $ Address (Bucket . T.pack $ b) (Key . T.pack $ k)

s3Parser'' :: Parser Address
s3Parser'' = do
  _ <- string "s3://"
  b <- takeWhile (/= '/')
  endOfInput
  pure $ Address (Bucket b) (Key "")
