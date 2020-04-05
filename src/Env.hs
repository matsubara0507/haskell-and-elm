{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Env
    ( App
    , convert
    , Env
    , TodoDB
    , OAuthSettings
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Data.Extensible
import           Data.IntMap            (IntMap)
import           Servant.Auth.Server
import           Servant.Server         (Handler)
import qualified Text.Blaze.Html5       as H
import           Todo                   (Todo)

type App = ReaderT Env Handler

convert :: Env -> App a -> Handler a
convert context app = runReaderT app context

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
