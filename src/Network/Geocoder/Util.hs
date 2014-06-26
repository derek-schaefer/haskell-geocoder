module Network.Geocoder.Util
    ( maybeCons
    , buildURL
    , maybeGet
    , maybeGetJSON
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP (urlEncodeVars)
import Control.Concurrent.Async
import Data.Aeson (FromJSON(..), decode)
import qualified Data.ByteString.Lazy.Char8 as BS

maybeCons :: Maybe a -> (a -> b) -> [b] -> [b]
maybeCons m f l =
    case m of
      Nothing -> l
      Just a' -> (f a') : l

buildURL :: String -> [(String, String)] -> String
buildURL url params = url ++ "?" ++ urlEncodeVars params

maybeGet :: String -> IO (Maybe BS.ByteString)
maybeGet url = do
  a <- async $ simpleHttp url
  rsp <- wait a -- TODO: Return Async (Maybe BS)
  return $ Just rsp

maybeGetJSON :: (FromJSON a) => String -> IO (Maybe a)
maybeGetJSON url = do
  mbody <- maybeGet url
  case mbody of
    Nothing -> return Nothing
    Just body -> return $ decode body
