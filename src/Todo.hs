{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Todo where

import           Data.Aeson     (ToJSON)
import           Data.Proxy     (Proxy (..))
import           Elm            (ElmType)
import           GHC.Generics   (Generic)
import           Servant.API    ((:>), Get, JSON)


type CRUD = "todos" :> Get '[JSON] [Todo]

crud :: Proxy CRUD
crud = Proxy

data Todo = Todo
  { todoId :: Int
  , title  :: String
  , done   :: Bool
  } deriving (Generic, Show)

instance ToJSON Todo
instance ElmType Todo
