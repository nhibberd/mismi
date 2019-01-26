{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.SecretsManager.Data (
    SecretPath (..)
  , SecretString (..)
  , SecretToken (..)
  , newSecretToken

  , CreateStringResult (..)
  , WriteStringResult (..)
  ) where


import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import           P

newtype SecretPath =
  SecretPath {
      renderSecretPath :: Text
    } deriving (Eq, Show)

newtype SecretString =
  SecretString {
      renderSecretString :: Text
    } deriving (Eq, Show)

newtype SecretToken =
  SecretToken {
      renderSecretToken :: Text
    } deriving (Eq, Show)


newSecretToken :: IO SecretToken
newSecretToken =
  UUID.nextRandom >>=
    pure . SecretToken . UUID.toText

data CreateStringResult =
    CreateStringSuccess
  | CreateStringAlreadyExists
    deriving (Eq, Show)

data WriteStringResult =
    WriteStringSuccess
  | WriteStringMissing
    deriving (Eq, Show)
