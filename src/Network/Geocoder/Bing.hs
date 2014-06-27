{-# LANGUAGE OverloadedStrings #-}

module Network.Geocoder.Bing
    ( BingGeocoder(..)
    , Response(..)
    , ResultSet(..)
    , Result(..)
    , Address(..)
    , Point(..)
    ) where

import Network.Geocoder hiding (Address)
import Network.Geocoder.Util
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))

baseURL = "https://dev.virtualearth.net/REST/v1/Locations"

-- BingGeocoder

data BingGeocoder = BingGeocoder {
      key :: String
    } deriving (Eq, Show)

getEncodeResponse :: BingGeocoder -> String -> IO (Maybe Response)
getEncodeResponse g loc = maybeGetJSON $ buildURL baseURL params
    where params = [("q", loc), ("o", "json"), ("key", key g)]

instance Geocoder BingGeocoder where

    encode g loc = do
      mrsp <- getEncodeResponse g loc
      case mrsp of
        Nothing -> return []
        Just rsp ->
            case resultSet rsp of
              (r:rs) -> return $ map resultLocation $ results r
              _      -> return []

    encode' g loc = encode g $ addressStr loc

-- Response

data Response = Response {
      traceID :: String
    , statusCode :: Int
    , statusDescription :: String
    , authenticationResultCode :: String
    , brandLogoURI :: String
    , copyright :: String
    , resultSet :: [ResultSet]
    } deriving (Eq, Show)

instance FromJSON Response where
    parseJSON (Object v) = Response
      <$> v .: "traceId"
      <*> v .: "statusCode"
      <*> v .: "statusDescription"
      <*> v .: "authenticationResultCode"
      <*> v .: "brandLogoUri"
      <*> v .: "copyright"
      <*> v .: "resourceSets"

-- ResultSet

data ResultSet = ResultSet {
      estimatedTotal :: Int,
      results :: [Result]
    } deriving (Eq, Show)

instance FromJSON ResultSet where
    parseJSON (Object v) = ResultSet
      <$> v .: "estimatedTotal"
      <*> v .: "resources"

-- Result

data Result = Result {
      resultType :: String
    , bbox :: (Double, Double, Double, Double)
    , name :: String
    , point :: Point
    , address :: Address
    , confidence :: String
    , entityType :: String
    , geocodePoints :: [Point]
    , matchCodes :: [String]
    } deriving (Eq, Show)

resultLocation :: Result -> Location
resultLocation r = pointLocation $ point r

instance FromJSON Result where
    parseJSON (Object v) = Result
      <$> v .: "__type"
      <*> v .: "bbox"
      <*> v .: "name"
      <*> v .: "point"
      <*> v .: "address"
      <*> v .: "confidence"
      <*> v .: "entityType"
      <*> v .: "geocodePoints"
      <*> v .: "matchCodes"

-- Point

data Point = Point {
      pointType :: String
    , coordinates :: (Double, Double)
    , calculationMethod :: Maybe String
    , usageTypes :: Maybe [String]
    } deriving (Eq, Show)

pointLocation :: Point -> Location
pointLocation p = Location { lat = fst coords, lng = snd coords }
    where coords = coordinates p

instance FromJSON Point where
    parseJSON (Object v) = Point
      <$> v .:  "type"
      <*> v .:  "coordinates"
      <*> v .:? "calculationMethod"
      <*> v .:? "usageTypes"

-- Address

data Address = Address {
      addressLine :: Maybe String
    , adminDistrict :: String
    , adminDistrict2 :: String
    , countryRegion :: String
    , formattedAddress :: String
    , locality :: String
    , postalCode :: Maybe String
    , neighborhood :: Maybe String
    , landmark :: Maybe String
    } deriving (Eq, Show)

instance FromJSON Address where
    parseJSON (Object v) = Address
      <$> v .:? "addressLine"
      <*> v .:  "adminDistrict"
      <*> v .:  "adminDistrict2"
      <*> v .:  "countryRegion"
      <*> v .:  "formattedAddress"
      <*> v .:  "locality"
      <*> v .:? "postalCode"
      <*> v .:? "neighborhood"
      <*> v .:? "landmark"
