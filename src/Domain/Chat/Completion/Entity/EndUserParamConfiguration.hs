{-# LANGUAGE DeriveGeneric #-}
module Domain.Chat.Completion.Entity.EndUserParamConfiguration where
import Data.Text (Text)
import Data.Aeson (Value, FromJSON)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)
data EndUserParamConfiguration = EndUserParamConfiguration
  { timezone :: Maybe Text
  , metadata :: Maybe Value
  } deriving (Generic, Show, Eq)

instance ToJSON EndUserParamConfiguration
instance FromJSON EndUserParamConfiguration