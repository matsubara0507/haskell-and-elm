{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Aeson
import           Data.Proxy               (Proxy (..))
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API              ((:<|>) (..), Get)
import           Servant.EDE              (HTML, loadTemplates)
import           Servant.Server           (Server, serve)
import           Todo                     (Todo (..))
import qualified Todo

main :: IO ()
main = do
  _ <- loadTemplates api [] "."
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server

type API = Get '[HTML "index.html"] Object
       :<|> Todo.CRUD

api :: Proxy API
api = Proxy

server :: Server API
server = index
     :<|> getTodos
  where
    index = pure mempty
    getTodos = pure todoList

todoList :: [Todo]
todoList =
  [ Todo 1 "アドベントカレンダーを書く" True
  , Todo 2 "Haskellで仕事する" False
  , Todo 3 "寝る" False
  ]
