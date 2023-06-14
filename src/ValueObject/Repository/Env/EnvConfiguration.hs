{-# LANGUAGE OverloadedStrings #-}
module ValueObject.Repository.Env.EnvConfiguration where
import Data.Text (Text)
import System.Environment (lookupEnv)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import ValueObject.Entity.OpenAIConfiguration (OpenAIConfiguration, newConfiguration)

fromEnvVariables :: IO (Either Text OpenAIConfiguration)
fromEnvVariables = do
    apiKeyValue <- lookupEnv "OPENAI_API_KEY"
    organizationValue <- lookupEnv "OPENAI_ORGANIZATION"
    case apiKeyValue of
        Nothing -> return $ Left "OPENAI_API_KEY environment variable not found"
        Just apiKey -> do
            let organization = T.pack $ fromMaybe "" organizationValue
            return $ Right $ newConfiguration  (T.pack apiKey) organization
