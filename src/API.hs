{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Aeson               (ToJSON)
import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic)
import           Servant.API              ((:>), Get, JSON)
import           Servant.Server           (Server)


type API = "todos" :> Get '[JSON] [Todo]

api :: Proxy API
api = Proxy

data Todo = Todo
  { todoId :: Int
  , title  :: String
  , done   :: Bool
  } deriving (Generic, Show)

instance ToJSON Todo

todoList :: [Todo]
todoList =
  [ Todo 1 "アドベントカレンダーを書く" True
  , Todo 2 "Haskellで仕事する" False
  , Todo 3 "寝る" False
  ]

server :: Server API
server = pure todoList
