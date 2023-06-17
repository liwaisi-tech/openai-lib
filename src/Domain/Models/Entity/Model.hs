{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.Model where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Domain.Models.Entity.ModelPermission (ModelPermission)
import Data.Text (Text)

data Model = Model
  { id :: Text
  , object :: Text
  , created :: Int
  , owned_by :: Text
  , permission :: [ModelPermission]
  , root :: Text
  , parent :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Model
instance ToJSON Model