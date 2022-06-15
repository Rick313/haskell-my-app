{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status404, status200, methodGet)
import Network.Wai.Handler.Warp (run)
import Data.Text (isPrefixOf, pack, unpack)
import System.Directory (doesFileExist)
import Middleware.Common (logger)
import Middleware.Router (route)

app :: Application
app request send = do
  let notfound = send . responseLBS status404 [("content-type", "text/plain")]
  case pathInfo request of
      -- Other routes ....
      otherwise -> notfound "Not found"

main = do
  putStrLn "Server started"
  putStrLn "http://localhost:3000"
  run 3000 $ logger app