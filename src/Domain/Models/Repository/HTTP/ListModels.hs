{-# LANGUAGE OverloadedStrings #-}
module Domain.Models.Repository.HTTP.ListModels where

import Data.Text (Text)
import ValueObject.Repository.HTTP.Constants (openAIBaseURL)
import Domain.Models.Entity.ModelList (ModelList)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client (parseRequest, Request (requestHeaders), httpLbs, Response (responseStatus, responseBody))

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (decode, eitherDecode)
import Network.HTTP.Types (statusIsSuccessful, statusIsClientError, statusIsServerError)
import Data.Either (fromRight)
import ValueObject.Entity.OpenAIConfiguration (OpenAIConfiguration(openAIAPIKey, openAIOrganization))
import ValueObject.Repository.Env.EnvConfiguration (fromEnvVariables)
import qualified Data.ByteString.Char8 as LBS
import ValueObject.Entity.Error (Error (_message))
import ValueObject.Entity.ErrorResponse (ErrorResponse (error))

listModelsUrl :: Text
listModelsUrl = openAIBaseURL <> "models"

-- GET implementation to list all models calling the operationId: listModels
listModels :: IO (Either Text ModelList)
listModels = do
    tlsManager <- newTlsManager
    request <- parseRequest $ T.unpack listModelsUrl
    eitherConfiguration <- fromEnvVariables
    case eitherConfiguration of
        Left error -> return $ Left error
        Right configuration -> do
            let authHeaders = LBS.pack $ "Bearer " <> T.unpack (openAIAPIKey configuration)
            let organizationHeader = LBS.pack $ T.unpack $ openAIOrganization configuration
            let requestWithHeaders = request { requestHeaders = [
                ("Authorization", authHeaders),
                ("OpenAI-Organization", organizationHeader)
            ] }
            response <- httpLbs requestWithHeaders tlsManager
            let body = TE.decodeUtf8 $ LBS.toStrict $ responseBody response
            let eitherError = eitherDecode $ responseBody response :: Either String ErrorResponse
            case responseStatus response of
                status | statusIsSuccessful status -> do
                    let eitherModelList = eitherDecode $ responseBody response :: Either String ModelList
                    case eitherModelList of
                        Right modelList -> return $ Right modelList
                        Left err -> return $ Left ("Error decoding response body to ModelList" <> T.pack err)
                status | statusIsClientError status ->
                    return $ do
                        case eitherError of
                            Right error -> Left $ "Client error when calling listModels: " <> _message error
                            Left err -> Left $ "Client error when calling listModels: " <> body  
                status | statusIsServerError status ->
                    return $ do
                        case eitherError of
                            Right error -> Left $ "Server error when calling listModels: " <> _message error
                            Left err -> Left $ "Server error when calling listModels: " <> body
                _ ->
                    return $ Left $ "Unknown error when calling listModels: " <> body
