{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow             (second)
import           Control.Concurrent.STM    (TVar, atomically, modifyTVar,
                                            newTVar, readTVar, writeTVar)
import           Control.Lens              ((&), (.~))
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (Object)
import           Data.Extensible           (emptyRecord, (<:), (@=))
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap
import           Data.Proxy                (Proxy (..))
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant.API               ((:<|>) (..), (:>), Get, Raw)
import           Servant.EDE               (HTML, loadTemplates)
import           Servant.Server            (Server, serve)
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           Todo                      (Todo)
import qualified Todo

main :: IO ()
main = do
  db <- atomically $ newTVar (length initTodoList, IntMap.fromList initTodoList)
  _ <- loadTemplates api [] "."
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api (server db)

type API = Get '[HTML "index.html"] Object
         :<|> "static" :> Raw
         :<|> Todo.CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap Todo) -> Server API
server db = index
     :<|> serveDirectoryFileServer "static"
     :<|> getTodos
     :<|> postTodo
     :<|> putTodoId
     :<|> deleteTodoId
  where
    index = pure mempty
    getTodos = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
    postTodo todo = liftIO . atomically $ do
      (maxId, m) <- readTVar db
      let
        newId = maxId + 1
        newTodo = todo & #id .~ newId
      writeTVar db (newId, IntMap.insert newId newTodo m)
      pure newTodo
    putTodoId tid todo =
      liftIO . atomically . modifyTVar db . second $ IntMap.insert tid todo
    deleteTodoId tid   =
      liftIO . atomically . modifyTVar db . second $ IntMap.delete tid

initTodoList :: [(Int, Todo)]
initTodoList =
  [ (1, #id @= 1 <: #title @= "アドベントカレンダーを書く" <: #done @= True <: emptyRecord)
  , (2, #id @= 2 <: #title @= "Haskellで仕事する" <: #done @= False <: emptyRecord)
  , (3, #id @= 3 <: #title @= "寝る" <: #done @= False <: emptyRecord)
  ]
