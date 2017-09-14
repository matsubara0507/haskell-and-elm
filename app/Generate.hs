{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Extensible.Instances
import           Data.Proxy                (Proxy (..))
import           Elm                       (Spec (Spec), specsToDir,
                                            toElmDecoderSource,
                                            toElmEncoderSource, toElmTypeSource)
import           Servant.Elm               (ElmOptions (..), UrlPrefix (Static),
                                            defElmImports, defElmOptions,
                                            generateElmForAPIWith)
import           Shelly                    (run_, shelly)
import           Todo                      (CRUD, Todo, example)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8080" }

spec :: Spec
spec = Spec ["Generated", "TodoAPI"]
            (defElmImports
             : toElmTypeSource    example
             : toElmDecoderSource example
             : toElmEncoderSource example
             : generateElmForAPIWith elmOpts  (Proxy :: Proxy CRUD))

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm-make" ["elm-src/Main.elm", "--output=static/main.js"]
