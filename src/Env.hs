{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Env
    ( Env
    , TodoDB
    , OAuthSettings
    ) where

import qualified Control.Concurrent.STM as STM
import           Data.Extensible
import           Data.IntMap            (IntMap)
import           Servant.Auth.Server
import qualified Text.Blaze.Html5       as H
import           Todo                   (Todo)

type Env = Record
  '[ "index"  >: H.Html
   , "cookie" >: CookieSettings
   , "jwt"    >: JWTSettings
   , "todos"  >: TodoDB
   , "oauth"  >: OAuthSettings
   ]

type TodoDB = STM.TVar (Int, IntMap Todo)

type OAuthSettings = Record
  '[ "client_id"     >: String
   , "client_secret" >: String
   ]
