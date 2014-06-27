module Network.Geocoder.Util
    ( catParams
    , buildURL
    , buildURL'
    , maybeGet
    , maybeGetJSON
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP (urlEncodeVars)
import Control.Concurrent.Async
import Data.Aeson (FromJSON(..), decode)
import qualified Data.ByteString.Lazy.Char8 as BS

catParams :: [(a, Maybe b)] -> [(a, b)]
catParams ts = [(t1, t2) | (t1, Just t2) <- ts]

buildURL :: String -> [(String, String)] -> String
buildURL url params = url ++ "?" ++ urlEncodeVars params

buildURL' :: String -> [(String, Maybe String)] -> String
buildURL' base params = base ++ "?" ++ (urlEncodeVars $ catParams params)

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
