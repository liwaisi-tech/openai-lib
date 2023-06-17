{-# LANGUAGE DeriveGeneric #-}
module Domain.Chat.Completion.Entity.ChatCompletionRequestMessage where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
    
data ChatCompletionRequestMessage = ChatCompletionRequestMessage
  { speaker :: Maybe Text
  , text :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON ChatCompletionRequestMessage
instance ToJSON ChatCompletionRequestMessage