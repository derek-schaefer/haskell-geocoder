{-# LANGUAGE OverloadedStrings #-}

module Network.Geocoder.Google
    ( GoogleGeocoder(..)
    , Response(..)
    , Result(..)
    , Geometry(..)
    , Viewport(..)
    , AddressComponent(..)
    ) where

import Network.Geocoder
import Network.Geocoder.Util
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), Value(..), (.:))

baseURL = "https://maps.googleapis.com/maps/api/geocode/json"

-- GoogleGeocoder

data GoogleGeocoder = GoogleGeocoder {
      key :: Maybe String
    } deriving (Eq, Show)

getEncodeResponse :: GoogleGeocoder -> String -> IO (Maybe Response)
getEncodeResponse g loc = maybeGetJSON $ buildURL' baseURL params
    where params = [("key", key g), ("address", Just loc)]

getDecodeResponse :: GoogleGeocoder -> Double -> Double -> IO (Maybe Response)
getDecodeResponse g lat lng = maybeGetJSON $ buildURL' baseURL params
    where latlng = (show lat) ++ "," ++ (show lng)
          params = [("key", key g), ("latlng", Just latlng)]

instance Geocoder GoogleGeocoder where

    encode g loc = do
      mrsp <- getEncodeResponse g loc
      case mrsp of
        Nothing  -> return []
        Just rsp -> return $ map loc' (results rsp)
            where loc' r = location . geometry $ r

    encode' g loc = encode g $ addressStr loc

    decode g lat lng = do
      mrsp <- getDecodeResponse g lat lng
      case mrsp of
        Nothing  -> return []
        Just rsp -> return $ map loc' (results rsp)
            where loc' r = formattedAddress r

-- Response

data Response = Response {
      status :: String,
      results :: [Result]
    } deriving (Eq, Show)

instance FromJSON Response where
    parseJSON (Object v) = Response
      <$> v .: "status"
      <*> v .: "results"

-- Result

data Result = Result {
      types :: [String]
    , formattedAddress :: String
    , addressComponents :: [AddressComponent]
    , geometry :: Geometry
    } deriving (Eq, Show)

instance FromJSON Result where
    parseJSON (Object v) = Result
      <$> v .: "types"
      <*> v .: "formatted_address"
      <*> v .: "address_components"
      <*> v .: "geometry"

-- Geometry

data Geometry = Geometry {
      location :: Location
    , locationType :: String
    , viewport :: Viewport
    } deriving (Eq, Show)

instance FromJSON Geometry where
    parseJSON (Object v) = Geometry
      <$> v .: "location"
      <*> v .: "location_type"
      <*> v .: "viewport"

-- Viewport

data Viewport = Viewport {
      northEast :: Location
    , southWest :: Location
    } deriving (Eq, Show)

instance FromJSON Viewport where
    parseJSON (Object v) = Viewport
      <$> v .: "northeast"
      <*> v .: "southwest"

-- AddressComponent

data AddressComponent = AddressComponent {
      longName :: String
    , shortName :: String
    , addressTypes :: [String]
    } deriving (Eq, Show)

instance FromJSON AddressComponent where
    parseJSON (Object v) = AddressComponent
      <$> v .: "long_name"
      <*> v .: "short_name"
      <*> v .: "types"
