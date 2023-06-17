{-# LANGUAGE DeriveGeneric #-}
module ValueObject.Entity.ErrorResponse where

import ValueObject.Entity.Error (Error)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ErrorResponse = ErrorResponse
  { error :: Error
  } deriving (Show, Generic, Eq)

instance FromJSON ErrorResponse
instance ToJSON ErrorResponse