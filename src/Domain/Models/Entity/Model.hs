{-# LANGUAGE DeriveGeneric #-}
module Domain.Models.Entity.Model where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Domain.Models.Entity.ModelPermission (ModelPermission)

data Model = Model
  { id :: String
  , object :: String
  , created :: Int
  , owned_by :: String
  , permission :: [ModelPermission]
  , root :: String
  , parent :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Model
instance ToJSON Model