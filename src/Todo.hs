{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Todo where

import           Data.Extensible             (type (>:), Record, nil, (<:),
                                              (@=))
import           Data.Extensible.Elm.Mapping (compileElmRecordAliasWith,
                                              compileElmRecordTypeWith)
import           Data.Proxy                  (Proxy (..))
import           Elm.Mapping                 (ETypeDef (ETypeAlias),
                                              IsElmDefinition (..),
                                              IsElmType (..))
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, FormUrlEncoded, Get, JSON,
                                              Post, Put, ReqBody)

type Todo = Record
  '[ "id" >: Int
   , "title" >: String
   , "done" >: Bool
   ]

instance IsElmType Todo where
  compileElmType = compileElmRecordTypeWith "Todo"

instance IsElmDefinition Todo where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Todo"

example :: Todo
example = #id @= 1
       <: #title @= "hoge"
       <: #done @= True
       <: nil

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
       :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
       :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
