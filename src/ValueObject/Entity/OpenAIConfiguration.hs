module ValueObject.Entity.OpenAIConfiguration where

import Data.Text (Text)

data OpenAIConfiguration = Configuration
  { openAIAPIKey :: Text
  , openAIOrganization :: Text
  } deriving (Show, Eq)

newConfiguration :: Text -> Text -> OpenAIConfiguration
newConfiguration = Configuration