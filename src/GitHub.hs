{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module GitHub
  ( Account
  , User
  , authorizeUrl
  , fetchUser
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON, toJSON)
import           Data.ByteString        (ByteString)
import           Data.Extensible
import           Data.String            (IsString (..))
import           Env                    (OAuthSettings)
import           Lens.Micro             ((&), (.~), (?~), (^.))
import qualified Network.Wreq           as W
import           Servant.Auth.Server    (FromJWT, ToJWT)

newtype Account = Account String
  deriving (Eq, Read, Ord, FromJSON, ToJSON, IsString)

type User = Record
  '[ "login" >: Account
   , "email" >: Maybe String
   ]

instance ToJWT User
instance FromJWT User

authorizeUrl :: OAuthSettings -> String
authorizeUrl settings =
  "https://github.com/login/oauth/authorize?client_id=" ++ settings ^. #client_id

fetchUser :: MonadIO m => OAuthSettings -> String -> m (Maybe User)
fetchUser settings code =
  fetchToken (shrink $ #code @= code <: settings) >>= \case
    Just token -> (^. W.responseBody) <$> liftIO (W.asJSON =<< fetch' token)
    Nothing    -> pure Nothing
  where
    fetch' t = W.getWith (W.defaults & W.auth ?~ W.oauth2Bearer t) url
    url      = "https://api.github.com/user"

fetchToken ::  MonadIO m => TokenParams -> m (Maybe ByteString)
fetchToken params = do
  res <- liftIO $ W.asJSON =<< W.postWith opts url (toJSON params)
  pure $ toToken <$> res ^. W.responseBody
  where
    opts = W.defaults & W.header "Accept" .~ ["application/json"]
    url  = "https://github.com/login/oauth/access_token"

toToken :: TokenInfo -> ByteString
toToken info = fromString $ info ^. #access_token

type TokenInfo = Record '[ "access_token" >: String ]

type TokenParams = Record
  '[ "client_id"     >: String
   , "client_secret" >: String
   , "code"          >: String
   ]
