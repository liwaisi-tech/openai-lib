{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.ModelPermission where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data ModelPermission = ModelPermission
  { id :: Text
  , object :: Text
  , created :: Int
  , allow_create_engine :: Bool
  , allow_sampling :: Bool
  , allow_logprobs :: Bool
  , allow_search_indices :: Bool
  , allow_view :: Bool
  , allow_fine_tuning :: Bool
  , organization :: Text
  , group :: Maybe Text
  , is_blocking :: Bool
  } deriving (Show, Generic, Eq)

instance FromJSON ModelPermission
instance ToJSON ModelPermission