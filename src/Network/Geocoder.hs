{-# LANGUAGE OverloadedStrings  #-}

module Network.Geocoder
    ( Geocoder(..)
    , Address(..)
    , Location(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
    ( FromJSON(..), ToJSON(..), Value(..),
      (.:), (.:?), (.=), object )

data Address = Address {
      line1 :: String,
      line2 :: Maybe String,
      city :: String,
      postal :: String,
      province :: String,
      country :: String
    } deriving (Eq, Show)

addressStr :: Address -> String
addressStr a = "asdf"

instance FromJSON Address where
    parseJSON (Object v) =
        Address <$> v .:  "line1"
                <*> v .:? "line2"
                <*> v .:  "city"
                <*> v .:  "postal"
                <*> v .:  "province"
                <*> v .:  "country"

instance ToJSON Address where
    toJSON (Address line1 line2 city province zipcode country) =
        object [ "line1"    .= line1
               , "line2"    .= line2
               , "city"     .= city
               , "province" .= province
               , "zipcode"  .= zipcode
               , "country"  .= country ]

data Location = Location {
      lat :: Double
    , lng :: Double
    } deriving (Eq, Show)

instance FromJSON Location where
    parseJSON (Object v) =
        Location <$> v .: "lat"
                 <*> v .: "lng"

instance ToJSON Location where
    toJSON (Location lat lng) =
        object [ "lat" .= lat
               , "lng" .= lng ]

class Geocoder g where
    --encode :: g -> Address -> IO [Location]
    encodeStr :: g -> String -> IO [Location]
    --decode :: g -> Location -> IO [String]
