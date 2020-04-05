{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           API                         (API, server)
import qualified Control.Concurrent.STM      as STM
import           Data.Extensible             (nil, (<:), (@=))
import qualified Data.IntMap                 as IntMap
import           Data.Proxy                  (Proxy (..))
import           Data.String                 (fromString)
import qualified Data.Time                   as Time
import qualified Network.Wai.Handler.Warp    as Warp
import           Orphans                     ()
import           Servant.Auth.Server
import           Servant.Server
import           System.Environment          (getEnv)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H
import           Todo                        (Todo)

main :: IO ()
main = do
  todoDB <- STM.atomically $ STM.newTVar (length initTodos, IntMap.fromList initTodos)
  userDB <- STM.atomically $ STM.newTVar mempty
  clientId <- fromString <$> getEnv "GITHUB_CLIENT_ID"
  jwtSettings <- defaultJWTSettings <$> generateKey
  let cfg = cookieSettings :. jwtSettings :. EmptyContext
      env = #index  @= indexHtml
         <: #cookie @= cookieSettings
         <: #jwt    @= jwtSettings
         <: #todos  @= todoDB
         <: #users  @= userDB
         <: #oauth  @= (#client_id @= clientId <: #client_secret @= "" <: nil)
         <: nil
  putStrLn "Listening on port 8080"
  Warp.run 8080 $
    serveWithContext api cfg (server env)
  where
    cookieSettings = defaultCookieSettings
      { cookieIsSecure = NotSecure -- localhost only
      , cookieMaxAge = Just $ Time.secondsToDiffTime (3 * 60)
      , cookieXsrfSetting = Nothing
      }

api :: Proxy (API '[Cookie])
api = Proxy

ctx :: Proxy '[ CookieSettings, JWTSettings]
ctx = Proxy

indexHtml :: H.Html
indexHtml = H.docTypeHtml $ do
  H.head $ stylesheet primerCss
  H.head $ stylesheet fontAwesomeCss
  H.div ! H.id "main" $ H.text ""
  H.script ! H.src "static/main.js" $ H.text ""
  H.script ! H.src "static/index.js" $ H.text ""
  where
    primerCss = "https://unpkg.com/@primer/css@14.3.0/dist/primer.css"
    fontAwesomeCss = "https://use.fontawesome.com/releases/v5.13.0/css/all.css"

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"

initTodos :: [(Int, Todo)]
initTodos =
  [ (1, #id @= 1 <: #title @= "アドベントカレンダーを書く" <: #done @= True <: nil)
  , (2, #id @= 2 <: #title @= "Haskellで仕事する" <: #done @= False <: nil)
  , (3, #id @= 3 <: #title @= "寝る" <: #done @= False <: nil)
  ]
