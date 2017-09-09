{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Todo where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Proxy                  (Proxy (..))
import           Elm                         (ElmType)
import           GHC.Generics                (Generic)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, FormUrlEncoded, Get, JSON,
                                              Post, Put, ReqBody)
import           Web.Internal.FormUrlEncoded (FromForm)

data Todo = Todo
  { todoId :: Int
  , title  :: String
  , done   :: Bool
  } deriving (Generic, Show)

instance FromJSON Todo
instance ToJSON Todo
instance FromForm Todo
instance ElmType Todo

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
       :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
       :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
