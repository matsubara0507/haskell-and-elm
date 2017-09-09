{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy  (Proxy (..))
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmTypeSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), UrlPrefix (Static),
                              defElmImports, defElmOptions,
                              generateElmForAPIWith)
import           Shelly      (run_, shelly)
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
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm-make" ["elm-src/Main.elm"]
