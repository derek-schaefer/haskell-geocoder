{-# LANGUAGE OverloadedStrings #-}

module Network.Geocoder.Google
    ( GoogleGeocoder(..)
    , GoogleResponse(..)
    , GoogleResult(..)
    , GoogleGeometry(..)
    , GoogleViewport(..)
    , GoogleAddressComponent(..)
    ) where

import Network.Geocoder
import Network.Geocoder.Util
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), Value(..), (.:))

baseURL = "https://maps.googleapis.com/maps/api/geocode/json?"

-- GoogleGeocoder

data GoogleGeocoder = GoogleGeocoder {
      key :: Maybe String
    } deriving (Eq, Show)

instance Geocoder GoogleGeocoder where
    encodeStr g loc = do
      let keyParam = maybeCons (key g) (\k -> ("key", k)) []
          uri = buildURL baseURL $ keyParam ++ [("address", loc)]
      mrsp <- (maybeGetJSON uri :: IO (Maybe GoogleResponse))
      case mrsp of
        Nothing -> return []
        Just rsp -> return $ map (\r -> location $ geometry r) $ results rsp

-- GoogleResponse

data GoogleResponse = GoogleResponse {
      status :: String,
      results :: [GoogleResult]
    } deriving (Eq, Show)

instance FromJSON GoogleResponse where
    parseJSON (Object v) = GoogleResponse
      <$> v .: "status"
      <*> v .: "results"

-- GoogleResult

data GoogleResult = GoogleResult {
      types :: [String]
    , formattedAddress :: String
    , addressComponents :: [GoogleAddressComponent]
    , geometry :: GoogleGeometry
    } deriving (Eq, Show)

instance FromJSON GoogleResult where
    parseJSON (Object v) = GoogleResult
      <$> v .: "types"
      <*> v .: "formatted_address"
      <*> v .: "address_components"
      <*> v .: "geometry"

-- GoogleGeometry

data GoogleGeometry = GoogleGeometry {
      location :: Location
    , locationType :: String
    , viewport :: GoogleViewport
    } deriving (Eq, Show)

instance FromJSON GoogleGeometry where
    parseJSON (Object v) = GoogleGeometry
      <$> v .: "location"
      <*> v .: "location_type"
      <*> v .: "viewport"

-- GoogleViewport

data GoogleViewport = GoogleViewport {
      northEast :: Location
    , southWest :: Location
    } deriving (Eq, Show)

instance FromJSON GoogleViewport where
    parseJSON (Object v) = GoogleViewport
      <$> v .: "northeast"
      <*> v .: "southwest"

-- GoogleAddressComponent

data GoogleAddressComponent = GoogleAddressComponent {
      longName :: String
    , shortName :: String
    , addressTypes :: [String]
    } deriving (Eq, Show)

instance FromJSON GoogleAddressComponent where
    parseJSON (Object v) = GoogleAddressComponent
      <$> v .: "long_name"
      <*> v .: "short_name"
      <*> v .: "types"
