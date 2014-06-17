module Data.Geocoder.Google (GoogleGeocoder) where

import Data.Geocoder

data GoogleGeocoder = GoogleGeocoder

instance Geocoder GoogleGeocoder where
    encodeStr loc = return $ Location { lat = 1, lng = 2 }
