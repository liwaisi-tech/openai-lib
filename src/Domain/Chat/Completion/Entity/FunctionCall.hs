{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.Chat.Completion.Entity.FunctionCall where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, Value (String, Object), object, KeyValue ((.=)), (.:))
import Data.Aeson.Types (FromJSON(parseJSON))
data FunctionCall = None | Auto | FunctionCall { functionName :: Text } deriving (Generic, Show, Eq)

instance ToJSON FunctionCall where
  toJSON None = String "none"
  toJSON Auto = String "auto"
  toJSON (FunctionCall name) = object ["name" .= name]

instance FromJSON FunctionCall where
  parseJSON (String "none") = pure None
  parseJSON (String "auto") = pure Auto
  parseJSON (Object o) = FunctionCall <$> o .: "name"
  parseJSON _ = fail "Invalid FunctionCall"
