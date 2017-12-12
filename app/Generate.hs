{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy
import           Elm
import           Servant.Elm
import           Shelly      (mkdir_p, run_, shelly)
import           Todo        (CRUD, Todo)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8080" }

spec :: Spec
spec = Spec ["Generated", "TodoAPI"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Todo)
             : toElmDecoderSource (Proxy :: Proxy Todo)
             : toElmEncoderSource (Proxy :: Proxy Todo)
             : generateElmForAPIWith elmOpts  (Proxy :: Proxy CRUD))

main :: IO ()
main = do
  shelly $ mkdir_p "elm-src/Generated"
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm-make" ["elm-src/Main.elm", "--output=static/main.js"]
