{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API
  ( API
  , server
  ) where

import           Control.Arrow              (second)
import qualified Control.Concurrent.STM     as STM
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.IntMap                as IntMap
import           Env
import qualified GitHub
import           Lens.Micro                 ((&), (.~), (^.))
import           Servant.API
import           Servant.Auth.Server
import           Servant.HTML.Blaze
import           Servant.Server
import           Servant.Server.StaticFiles (serveDirectoryFileServer)
import qualified Text.Blaze.Html5           as H
import qualified Todo

type API auth = (Auth auth GitHub.User :> Protected) :<|> Unprotected

type Protected
      = "api" :> Todo.CRUD

type Unprotected
      = Get '[HTML] H.Html
   :<|> "static"   :> Raw
   :<|> "login"    :> GetRedirected '[]
   :<|> "callback" :> QueryParam "code" String :> GetRedirected JWTCookieHeaders

type GetRedirected headers =
  Verb 'GET 308 '[HTML] (Headers (Header "Location" String ': headers) NoContent)

type JWTCookieHeaders =
  '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie ]

server :: Env -> Server (API auths)
server env = protected env :<|> unprotected env

protected :: Env -> AuthResult GitHub.User -> Server Protected
protected env (Authenticated _) = todoAPI (env ^. #todos)
protected _ _                   = pure [] :<|> throwAll err401

todoAPI :: TodoDB -> Server Todo.CRUD
todoAPI db = getTodos :<|> postTodo :<|> putTodoId :<|> deleteTodoId
  where
    getTodos = liftIO $ IntMap.elems . snd <$> STM.atomically (STM.readTVar db)
    postTodo todo = liftIO . STM.atomically $ do
      (maxId, m) <- STM.readTVar db
      let newId = maxId + 1
          newTodo = todo & #id .~ newId
      STM.writeTVar db (newId, IntMap.insert newId newTodo m)
      pure newTodo
    putTodoId tid todo =
      liftIO . STM.atomically . STM.modifyTVar db . second $ IntMap.insert tid todo
    deleteTodoId tid   =
      liftIO . STM.atomically . STM.modifyTVar db . second $ IntMap.delete tid

unprotected :: Env -> Server Unprotected
unprotected env =
  pure (env ^. #index) :<|> serveDirectoryFileServer "static" :<|> login :<|> callback
  where
    login = pure $ addHeader (GitHub.authorizeUrl $ env ^. #oauth) NoContent
    callback (Just code) = GitHub.fetchUser (env ^. #oauth) code >>= \case
      Nothing   -> throwError err401
      Just user -> liftIO (acceptLogin (env ^. #cookie) (env ^. #jwt) user) >>= \case
        Nothing           -> throwError err401
        Just applyCookies -> pure $ addHeader "/" (applyCookies NoContent)
    callback _ = throwError err401
