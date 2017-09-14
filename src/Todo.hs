{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo where

import           Data.Extensible             (type (>:), Record, emptyRecord,
                                              (<:), (@=))
import           Data.Extensible.Instances   ()
import           Data.Proxy                  (Proxy (..))
import           Elm                         (ElmType (..))
import           Elm.Class
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, FormUrlEncoded, Get, JSON,
                                              Post, Put, ReqBody)

type Todo = Record
  '[ "id" >: Int
   , "title" >: String
   , "done" >: Bool
   ]

instance ElmType Todo where
  toElmType = toElmRecordType "Todo"

example :: Todo
example = #id @= 1
       <: #title @= "hoge"
       <: #done @= True
       <: emptyRecord

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
       :<|> "todos" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
       :<|> "todos" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
