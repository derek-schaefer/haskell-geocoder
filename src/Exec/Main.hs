module Main where

import System.Environment
import Network.Geocoder
import Network.Geocoder.Google

main :: IO ()
main = do
  let geo = GoogleGeocoder { key = Nothing }
  args <- getArgs
  mapM_ (\a -> do { loc <- getLocationStr geo a; putStrLn $ show loc }) args

getLocationStr :: (Geocoder g) => g -> String -> IO [String]
getLocationStr geo addr = do
  locs <- encode geo addr
  return $ map show locs
