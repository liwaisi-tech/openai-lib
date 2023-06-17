{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.ListModelsResponse where
import Domain.Models.Entity.Model (Model)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Options (fieldLabelModifier), genericParseJSON, defaultOptions, genericToJSON)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON (toJSON))

data ListModelsResponse = ListModelsResponse
  { _data :: [Model]
  , _object :: String
  } deriving (Show, Generic)

instance FromJSON ListModelsResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ListModelsResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
