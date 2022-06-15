module Middleware.Common (logger, static) where

import Network.Wai
import Network.HTTP.Types (status200)
import System.Directory (doesFileExist)
import Data.Text (isPrefixOf, pack, unpack)

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