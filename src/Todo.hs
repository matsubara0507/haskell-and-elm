{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Todo where

import           Data.Aeson
import           Elm
import           GHC.Generics
import           Servant.API
import           Web.Internal.FormUrlEncoded (FromForm)

data Todo = Todo
  { todoId :: Int
  , title  :: String
  , done   :: Bool
  } deriving (Generic)

instance FromJSON Todo
instance ToJSON Todo
instance FromForm Todo
instance ElmType Todo

type CRUD
    = "todos" :> Get '[JSON] [Todo]
 :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
 :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
 :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()
