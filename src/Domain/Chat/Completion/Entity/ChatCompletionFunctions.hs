{-# LANGUAGE DeriveGeneric #-}
module Domain.Chat.Completion.Entity.ChatCompletionFunctions where
import Data.Text (Text)
import Data.Aeson (Value, FromJSON, ToJSON)
import GHC.Generics (Generic)
data ChatCompletionFunctions = ChatCompletionFunctions
  { name :: Text
  , args :: Maybe [Value]
  } deriving (Generic, Show, Eq)

instance FromJSON ChatCompletionFunctions
instance ToJSON ChatCompletionFunctions