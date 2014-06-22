{-# LANGUAGE OverloadedStrings  #-}

module Network.Geocoder
    ( Geocoder(..)
    , Address(..)
    , Location(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), Value(..), (.:))

data Address = Address {
      value :: String
    } deriving (Eq, Show)

instance FromJSON Address where
    parseJSON (Object v) = Address
      <$> v .: "value"

data Location = Location {
      lat :: Double
    , lng :: Double
    } deriving (Eq, Show)

instance FromJSON Location where
    parseJSON (Object v) = Location
      <$> v .: "lat"
      <*> v .: "lng"

class Geocoder g where
    encodeStr :: g -> String -> IO (Maybe Location)
