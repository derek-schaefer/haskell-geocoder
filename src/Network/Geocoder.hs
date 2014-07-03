{-# LANGUAGE OverloadedStrings  #-}

module Network.Geocoder
    ( Geocoder(..)
    , Location(..)
    , Address(..)
    , addressStr
    ) where

import Data.List (intercalate)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson
    ( FromJSON(..), ToJSON(..), Value(..),
      (.:), (.:?), (.=), object )

-- Geocoder

class Geocoder g where
    encode  :: g -> String -> IO [Location]
    encode' :: g -> Address -> IO [Location]
    decode  :: g -> Double -> Double -> IO [String] -- TODO: IO [Address]
    --decode' :: g -> Location -> IO [Address]

-- Location

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

-- Address

data Address = Address {
      line1 :: String,
      line2 :: Maybe String,
      city :: String,
      postal :: String,
      province :: String,
      country :: String
    } deriving (Eq, Show)

addressStr :: Address -> String
addressStr a = intercalate ", " $ map elemToStr elems
    where elems = [Just (line1 a), line2 a, Just (city a), Just (postal a),
                   Just (province a), Just (country a)]
          elemToStr e = case e of
                          Nothing -> ""
                          Just e  -> show e

instance FromJSON Address where
    parseJSON (Object v) =
        Address <$> v .:  "line1"
                <*> v .:? "line2"
                <*> v .:  "city"
                <*> v .:  "postal"
                <*> v .:  "province"
                <*> v .:  "country"

instance ToJSON Address where
    toJSON (Address line1 line2 city postal province country) =
        object [ "line1"    .= line1
               , "line2"    .= line2
               , "city"     .= city
               , "postal"   .= postal
               , "province" .= province
               , "country"  .= country ]
