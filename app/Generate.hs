{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Elm.Mapping
import           Servant             ((:>))
import           Servant.Elm.Mapping (defElmImports, defElmOptions,
                                      generateElmModuleWith)
import           System.Process      (system)
import           Todo                (CRUD, Todo)

main :: IO ()
main = do
  generateElmModuleWith
    defElmOptions
    ["Generated", "API"]
    defElmImports
    "elm-src"
    [DefineElm (Proxy @ Todo)]
    (Proxy @ ("api" :> CRUD))
  mapM_ system
    [ "elm make elm-src/Main.elm --output=static/main.js"
    , "elm-format --yes elm-src/Generated/"
    ]
