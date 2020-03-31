{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Arrow               (second)
import           Control.Concurrent.STM      (TVar, atomically, modifyTVar,
                                              newTVar, readTVar, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Extensible             (emptyRecord, (<:), (@=))
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IntMap
import           Data.Proxy                  (Proxy (..))
import           Lens.Micro                  ((&), (.~))
import qualified Network.Wai.Handler.Warp    as Warp
import           Orphans                     ()
import           Servant.API                 ((:<|>) (..), (:>), Get, Raw)
import           Servant.HTML.Blaze
import           Servant.Server              (Server, serve)
import           Servant.Server.StaticFiles  (serveDirectoryFileServer)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H
import           Todo                        (Todo)
import qualified Todo

main :: IO ()
main = do
  db <- atomically $ newTVar (length initTodoList, IntMap.fromList initTodoList)
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api (server db)

type API = Get '[HTML] H.Html
         :<|> "static" :> Raw
         :<|> "api" :> Todo.CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap Todo) -> Server API
server db = indexHtml
     :<|> serveDirectoryFileServer "static"
     :<|> getTodos
     :<|> postTodo
     :<|> putTodoId
     :<|> deleteTodoId
  where
    indexHtml = pure $ H.docTypeHtml $ do
      H.head $ stylesheet primerCss
      H.head $ stylesheet fontAwesomeCss
      H.div ! H.id "main" $ H.text ""
      H.script ! H.src "static/main.js" $ H.text ""
      H.script ! H.src "static/index.js" $ H.text ""
    primerCss = "https://unpkg.com/@primer/css@14.3.0/dist/primer.css"
    fontAwesomeCss = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"

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

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"

initTodoList :: [(Int, Todo)]
initTodoList =
  [ (1, #id @= 1 <: #title @= "アドベントカレンダーを書く" <: #done @= True <: emptyRecord)
  , (2, #id @= 2 <: #title @= "Haskellで仕事する" <: #done @= False <: emptyRecord)
  , (3, #id @= 3 <: #title @= "寝る" <: #done @= False <: emptyRecord)
  ]
