{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Env
    ( Env
    , TodoDB
    , UserDB
    , Account
    , User
    ) where

import qualified Control.Concurrent.STM as STM
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString        (ByteString)
import           Data.Extensible
import           Data.IntMap            (IntMap)
import           Data.Map.Strict        (Map)
import           Data.String            (IsString)
import           Servant.Auth.Server
import qualified Text.Blaze.Html5       as H
import           Todo                   (Todo)

type Env = Record
  '[ "index"  >: H.Html
   , "cookie" >: CookieSettings
   , "jwt"    >: JWTSettings
   , "todos"  >: TodoDB
   , "users"  >: UserDB
   , "oauth"  >: OAuthSettings
   ]

type TodoDB = STM.TVar (Int, IntMap Todo)

type UserDB = STM.TVar (Map Account User)

newtype Account = Account String
  deriving (Eq, Read, Ord, FromJSON, ToJSON, IsString)

type User = Record
  '[ "account" >: Account
   , "email"   >: String
   ]

instance ToJWT User
instance FromJWT User

type OAuthSettings = Record
  '[ "client_id"     >: String
   , "client_secret" >: ByteString
   ]
