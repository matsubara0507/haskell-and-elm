{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module API
  ( API
  , GetRedirected
  , server
  ) where

import           Control.Arrow              (second)
import qualified Control.Concurrent.STM     as STM
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask, asks)
import qualified Data.IntMap                as IntMap
import           Env
import qualified GitHub
import           Lens.Micro                 ((&), (.~), (^.))
import           Lens.Micro.Extras          (view)
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

server :: ServerT (API auths) App
server = protected :<|> unprotected

protected :: AuthResult GitHub.User -> ServerT Protected App
protected (Authenticated _) = todoAPI
protected _                 = throwAll err401

todoAPI :: ServerT Todo.CRUD App
todoAPI = getTodos :<|> postTodo :<|> putTodoId :<|> deleteTodoId
  where
    getTodos = do
      db <- asks (view #todos)
      liftIO $ IntMap.elems . snd <$> STM.atomically (STM.readTVar db)
    postTodo todo = do
      db <- asks (view #todos)
      liftIO . STM.atomically $ do
        (maxId, m) <- STM.readTVar db
        let newId = maxId + 1
            newTodo = todo & #id .~ newId
        STM.writeTVar db (newId, IntMap.insert newId newTodo m)
        pure newTodo
    putTodoId tid todo = do
      db <- asks (view #todos)
      liftIO . STM.atomically . STM.modifyTVar db . second $ IntMap.insert tid todo
    deleteTodoId tid = do
      db <- asks (view #todos)
      liftIO . STM.atomically . STM.modifyTVar db . second $ IntMap.delete tid

unprotected :: ServerT Unprotected App
unprotected =
  asks (view #index) :<|> serveDirectoryFileServer "static" :<|> login :<|> callback
  where
    login = do
      oauth <- asks (view #oauth)
      pure $ addHeader (GitHub.authorizeUrl oauth) NoContent
    callback (Just code) = do
      env <- ask
      GitHub.fetchUser (env ^. #oauth) code >>= \case
        Nothing   -> throwError err401
        Just user -> liftIO (acceptLogin (env ^. #cookie) (env ^. #jwt) user) >>= \case
          Nothing           -> throwError err401
          Just applyCookies -> pure $ addHeader "/" (applyCookies NoContent)
    callback _ = throwError err401
