{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.ModelList where
import Domain.Models.Entity.Model (Model)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ModelList = ModelList
  { models :: [Model]
  , object :: String
  } deriving (Show, Generic)

instance FromJSON ModelList
instance ToJSON ModelList