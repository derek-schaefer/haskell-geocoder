module Data.Geocoder
    ( Address(Address)
    , Location(Location)
    , Geocoder
    ) where

data Address = Address {
      value :: String
    } deriving (Eq, Show)

data Location = Location {
      lat :: Double
    , lng :: Double
    } deriving (Eq, Show)

class Geocoder g where
    encodeStr :: g -> String -> IO Location
