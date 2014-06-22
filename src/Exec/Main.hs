module Main where

import Network.Geocoder
import Network.Geocoder.Google
import System.Environment
import qualified Data.List as L

main :: IO ()
main = do
  let geo = GoogleGeocoder { key = "" }
  args <- getArgs
  mapM_ (\a -> do { loc <- getLocationStr geo a; putStrLn loc }) args

getLocationStr :: (Geocoder g) => g -> String -> IO String
getLocationStr geo addr = do
  mloc <- encodeStr geo addr
  case mloc of
    Nothing -> return "Error"
    Just loc -> return $ show loc
