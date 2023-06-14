{-# LANGUAGE OverloadedStrings #-}
module Domain.Models.Repository.HTTP.ListModels where

import Data.Text (Text)
import ValueObject.Repository.HTTP.Constants (openAIBaseURL)
import Domain.Models.Entity.ModelList (ModelList)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client (parseRequest, Request (requestHeaders), httpLbs, Response (responseStatus, responseBody))
import Data.ByteString.Char8 as LBS (pack)
import qualified Data.Text as T
import Data.Aeson (decode)
import Network.HTTP.Types (statusIsSuccessful, statusIsClientError, statusIsServerError)
import Data.Either (fromRight)
import ValueObject.Entity.OpenAIConfiguration (OpenAIConfiguration(openAIAPIKey, openAIOrganization))
import ValueObject.Repository.Env.EnvConfiguration (fromEnvVariables)

listModelsUrl :: Text
listModelsUrl = openAIBaseURL <> "/models"

-- GET implementation to list all models calling the operationId: listModels
listModels :: IO (Either Text ModelList)
listModels = do
    tlsManager <- newTlsManager
    request <- parseRequest $ T.unpack listModelsUrl
    eitherConfiguration <- fromEnvVariables
    case eitherConfiguration of
        Left error -> return $ Left error
        Right configuration -> do
            let authHeaders = LBS.pack $ "Bareer " <> T.unpack (openAIAPIKey configuration)
            let organzationHeader = LBS.pack $ T.unpack $ openAIOrganization configuration
            let requestWithHeaders = request { requestHeaders = [
                ("Authorization", authHeaders),
                ("OpenAI-Organization", organzationHeader),
                ("Content-Type", "application/json")
            ] }
            response <- httpLbs requestWithHeaders tlsManager
            case responseStatus response of
                status | statusIsSuccessful status -> do
                    let maybeModelList = decode $ responseBody response
                    case maybeModelList of
                        Just modelList -> return $ Right modelList
                        Nothing -> return $ Left "Error decoding response body to ModelList"
                status | statusIsClientError status -> return $ Left "Client error when calling listModels"
                status | statusIsServerError status -> return $ Left "Server error when calling listModels"
                _ ->
                    return $ Left "Unknown error when calling listModels"
