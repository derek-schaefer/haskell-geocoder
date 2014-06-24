module Main where

import System.Environment
import Network.Geocoder
import qualified Network.Geocoder.Google as Google

main :: IO ()
main = do
  let geo = Google.GoogleGeocoder { Google.key = Nothing }
  args <- getArgs
  mapM_ (\a -> do { loc <- getLocationStr geo a; putStrLn $ show loc }) args

getLocationStr :: (Geocoder g) => g -> String -> IO [String]
getLocationStr geo addr = do
  locs <- encodeStr geo addr
  return $ map show locs
