{-# LANGUAGE DeriveGeneric #-}
module ValueObject.Entity.ErrorResponse where

import Domain.Models.Entity.Error (Error)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ErrorResponse = ErrorResponse
  { error :: Error
  } deriving (Show, Generic)

instance FromJSON ErrorResponse
instance ToJSON ErrorResponse