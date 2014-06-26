{-# LANGUAGE OverloadedStrings #-}

module Network.Geocoder.Bing
    ( BingGeocoder(..)
    , BingResponse(..)
    , BingResultSet(..)
    , BingResult(..)
    , BingAddress(..)
    , BingPoint(..)
    ) where

import Network.Geocoder
import Network.Geocoder.Util
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))

baseURL = "https://dev.virtualearth.net/REST/v1/Locations"

-- BingGeocoder

data BingGeocoder = BingGeocoder {
      key :: String
    } deriving (Eq, Show)

buildURL' :: BingGeocoder -> String -> String
buildURL' g loc = buildURL baseURL [("q", loc), ("o", "json"), ("key", key g)]

getResponse :: BingGeocoder -> String -> IO (Maybe BingResponse)
getResponse g loc = maybeGetJSON $ buildURL' g loc

instance Geocoder BingGeocoder where
    encodeStr g loc = do
      mrsp <- getResponse g loc
      case mrsp of
        Nothing -> return []
        Just rsp ->
            case resultSet rsp of
              (r:rs) -> return $ map resultLocation $ results r
              _      -> return []

-- BingResponse

data BingResponse = BingResponse {
      traceID :: String
    , statusCode :: Int
    , statusDescription :: String
    , authenticationResultCode :: String
    , brandLogoURI :: String
    , copyright :: String
    , resultSet :: [BingResultSet]
    } deriving (Eq, Show)

instance FromJSON BingResponse where
    parseJSON (Object v) = BingResponse
      <$> v .: "traceId"
      <*> v .: "statusCode"
      <*> v .: "statusDescription"
      <*> v .: "authenticationResultCode"
      <*> v .: "brandLogoUri"
      <*> v .: "copyright"
      <*> v .: "resourceSets"

-- BingResultSet

data BingResultSet = BingResultSet {
      estimatedTotal :: Int,
      results :: [BingResult]
    } deriving (Eq, Show)

instance FromJSON BingResultSet where
    parseJSON (Object v) = BingResultSet
      <$> v .: "estimatedTotal"
      <*> v .: "resources"

-- BingResult

data BingResult = BingResult {
      resultType :: String
    , bbox :: (Double, Double, Double, Double)
    , name :: String
    , point :: BingPoint
    , address :: BingAddress
    , confidence :: String
    , entityType :: String
    , geocodePoints :: [BingPoint]
    , matchCodes :: [String]
    } deriving (Eq, Show)

resultLocation :: BingResult -> Location
resultLocation r = pointLocation $ point r

instance FromJSON BingResult where
    parseJSON (Object v) = BingResult
      <$> v .: "__type"
      <*> v .: "bbox"
      <*> v .: "name"
      <*> v .: "point"
      <*> v .: "address"
      <*> v .: "confidence"
      <*> v .: "entityType"
      <*> v .: "geocodePoints"
      <*> v .: "matchCodes"

-- BingPoint

data BingPoint = BingPoint {
      pointType :: String
    , coordinates :: (Double, Double)
    , calculationMethod :: Maybe String
    , usageTypes :: Maybe [String]
    } deriving (Eq, Show)

pointLocation :: BingPoint -> Location
pointLocation p = Location { lat = fst coords, lng = snd coords }
    where coords = coordinates p

instance FromJSON BingPoint where
    parseJSON (Object v) = BingPoint
      <$> v .:  "type"
      <*> v .:  "coordinates"
      <*> v .:? "calculationMethod"
      <*> v .:? "usageTypes"

-- BingAddress

data BingAddress = BingAddress {
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

instance FromJSON BingAddress where
    parseJSON (Object v) = BingAddress
      <$> v .:? "addressLine"
      <*> v .:  "adminDistrict"
      <*> v .:  "adminDistrict2"
      <*> v .:  "countryRegion"
      <*> v .:  "formattedAddress"
      <*> v .:  "locality"
      <*> v .:? "postalCode"
      <*> v .:? "neighborhood"
      <*> v .:? "landmark"
