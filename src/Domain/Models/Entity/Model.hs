{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.Model where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Model = Model
  { id :: String
  , object :: String
  , owned_by :: String
  , permission :: [String]
  } deriving (Show, Generic)

instance FromJSON Model
instance ToJSON Model