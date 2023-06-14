{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.ModelPermission where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ModelPermission = ModelPermission
  { id :: String
  , object :: String
  , created :: Int
  , allow_create_engine :: Bool
  , allow_sampling :: Bool
  , allow_logprobs :: Bool
  , allow_search_indices :: Bool
  , allow_view :: Bool
  , allow_fine_tuning :: Bool
  , organization :: String
  , group :: Maybe String
  , is_blocking :: Bool
  } deriving (Show, Generic, Eq)

instance FromJSON ModelPermission
instance ToJSON ModelPermission