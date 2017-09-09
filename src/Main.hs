module Main where

import           API                      (api, server)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.Server           (serve)

main :: IO ()
main = do
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server
