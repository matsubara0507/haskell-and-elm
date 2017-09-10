{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow             (second)
import           Control.Concurrent.STM    (TVar, atomically, modifyTVar,
                                            newTVar, readTVar, writeTVar)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap
import           Data.Proxy                (Proxy (..))
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant.API               ((:<|>) (..), (:>), Get, Raw)
import           Servant.EDE               (HTML, loadTemplates)
import           Servant.Server            (Server, serve)
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           Todo                      (Todo (..))
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
        newTodo = todo { todoId = newId }
      writeTVar db (newId, IntMap.insert newId newTodo m)
      pure newTodo
    putTodoId tid todo =
      liftIO . atomically . modifyTVar db . second $ IntMap.insert tid todo
    deleteTodoId tid   =
      liftIO . atomically . modifyTVar db . second $ IntMap.delete tid

initTodoList :: [(Int, Todo)]
initTodoList =
  [ (1, Todo 1 "アドベントカレンダーを書く" True)
  , (2, Todo 2 "Haskellで仕事する" False)
  , (3, Todo 3 "寝る" False)
  ]
