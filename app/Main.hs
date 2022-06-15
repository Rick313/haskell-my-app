{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status404, status200)
import Network.Wai.Handler.Warp (run)
import Data.Text (isPrefixOf, pack, unpack)
import System.Directory (doesFileExist)

-- | Print each request "[Method HTTP] - pathname"
logger :: Middleware
logger app request send = do
  let path   = rawPathInfo request
      method = requestMethod request
  putStrLn "[Middleware] - My first middleware"
  putStrLn $ "["++ show method ++"] - " ++ show path
  app request send


-- | Serve static files
static :: String -> String -> Middleware
static routepath directory app request send = do
  let info     = pathInfo request
      path     = pack $ foldl (\acc x -> acc ++ "/" ++ unpack x) "" info
      filepath = directory ++ drop (length routepath) (unpack path)
      target   = if filepath == directory then directory ++ "/index.html" else filepath
  exist <- doesFileExist target
  if pack routepath `isPrefixOf` path && exist
    then send $ responseFile status200 [] target Nothing
    else app request send

app :: Application
app request send = do
  let notfound = send . responseLBS status404 [("content-type", "text/plain")]
  case pathInfo request of
      -- Other routes ....
      otherwise -> notfound "Not found"

main = do
  putStrLn "Server started"
  putStrLn "http://localhost:3000"
  run 3000 $ (logger {-  . static "/" "path/to/your/static/files" -}) app
