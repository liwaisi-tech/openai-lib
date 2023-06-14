{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.ModelList where
import Domain.Models.Entity.Model (Model)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Options (fieldLabelModifier), genericParseJSON, defaultOptions, genericToJSON)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON (toJSON))

data ModelList = ModelList
  { _data :: [Model]
  , _object :: String
  } deriving (Show, Generic)

instance FromJSON ModelList where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ModelList where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
