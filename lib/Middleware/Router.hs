{-# LANGUAGE OverloadedStrings #-}
module Middleware.Router (route) where

import Network.Wai
import Network.HTTP.Types (Method, status200)
import Data.Text (isPrefixOf, pack, split, empty)

comp :: (Eq x, Eq y) => (x -> y -> Bool) -> [x] -> [y] -> Bool
comp f xs ys
  | length xs /= length ys = False
  | otherwise =
      let x = head xs
          y = head ys
          result = f x y
      in if tail xs /= [] && tail ys /= [] && result
        then comp f (tail xs) (tail ys)
        else result

route :: [Method] -> String -> Middleware
route methods routepath app request send = do
  let method   = requestMethod request
      pathname = pathInfo request
      segments = filter (/= empty) $ split (== '/') $ pack routepath
      match    = comp (\x y -> ":" `isPrefixOf` y || y == x) pathname segments
      helper   = send . responseLBS status200 [("content-type", "text/plain")]
  if method `elem` methods && match
    then helper "Ok"
    else app request send
