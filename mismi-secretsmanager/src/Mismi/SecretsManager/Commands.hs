{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.SecretsManager.Commands (
    createString
  , readString
  , writeString

  , handleExists
  , handleMissing
  ) where

import           Control.Lens ((?~), view)

import           Mismi.Amazonka (send)
import           Mismi.Control
import           Mismi.SecretsManager.Data

import           Network.AWS.Types (ServiceError (..), ErrorCode (..))
import qualified Network.AWS.SecretsManager as A
import qualified Network.HTTP.Types.Status as HTTP

import           P

createString ::
     SecretPath
  -> SecretToken
  -> SecretString
  -> AWS CreateStringResult
createString path token string = do
  r <- handleExists .  send $
    A.createSecret (renderSecretPath path)
      & A.csSecretString ?~ renderSecretString string
      & A.csClientRequestToken ?~ renderSecretToken token
  case r of
    Nothing ->
      pure CreateStringAlreadyExists
    Just _ ->
      pure CreateStringSuccess

readString :: SecretPath -> AWS (Maybe SecretString)
readString path = do
  r <- handleMissing . send $
    A.getSecretValue (renderSecretPath path)

  pure . join . with r $
    fmap SecretString .
      view A.gsvrsSecretString

writeString ::
     SecretPath
  -> SecretToken
  -> SecretString
  -> AWS WriteStringResult
writeString path token string = do
  r <- handleMissing . send $
    A.putSecretValue (renderSecretPath path)
      & A.psvSecretString ?~ renderSecretString string
      & A.psvClientRequestToken ?~ renderSecretToken token
  case r of
    Just _ ->
      pure WriteStringSuccess
    Nothing ->
      pure WriteStringMissing


handleExists :: AWS a -> AWS (Maybe a)
handleExists =
  handleError "ResourceExists"

handleMissing :: AWS a -> AWS (Maybe a)
handleMissing =
  handleError "ResourceNotFound"

handleError :: ErrorCode -> AWS a -> AWS (Maybe a)
handleError code action =
  let
    check :: ServiceError -> Bool
    check er =
      let
        httpStatus = _serviceStatus er == HTTP.status400
        codeCheck = _serviceCode er == code
      in
        httpStatus && codeCheck
  in
    handleServiceError check (const Nothing) (Just <$> action)
