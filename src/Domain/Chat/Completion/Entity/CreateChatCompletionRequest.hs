{-# LANGUAGE DeriveGeneric #-}
module Domain.Chat.Completion.Entity.CreateChatCompletionRequest where
import Data.Text (Text)
import Domain.Chat.Completion.Entity.ChatCompletionRequestMessage
    ( ChatCompletionRequestMessage )
import Domain.Chat.Completion.Entity.ChatCompletionFunctions
    ( ChatCompletionFunctions )
import Domain.Chat.Completion.Entity.FunctionCall (FunctionCall)
import Domain.Chat.Completion.Entity.EndUserParamConfiguration 
    (EndUserParamConfiguration)
import Data.HashMap.Internal.Strict (HashMap)
import GHC.Generics (Generic)

data CreateChatCompletionRequest = CreateChatCompletionRequest
  { model :: Text
  , messages :: [ChatCompletionRequestMessage]
  , functions :: [ChatCompletionFunctions]
  , function_call :: Maybe FunctionCall
  , temperature :: Maybe Double
  , top_p :: Maybe Double
  , n :: Maybe Int
  , stream :: Maybe Bool
  , stop :: Maybe (Either Text [Text])
  , max_tokens :: Maybe Int
  , presence_penalty :: Maybe Double
  , frequency_penalty :: Maybe Double
  , logit_bias :: Maybe (HashMap Int Double)
  , user :: Maybe EndUserParamConfiguration
  } deriving (Generic, Show, Eq)