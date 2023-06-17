{-# LANGUAGE OverloadedStrings #-}
module Domain.Models.Repository.HTTP.RetrieveModel where
import Data.Text (Text)
import ValueObject.Repository.HTTP.Constants (openAIBaseURL)
import Domain.Models.Entity.Model (Model)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Data.Text as T
import Network.HTTP.Client (parseRequest, Request (requestHeaders), httpLbs, Response (responseBody, responseStatus))
import ValueObject.Repository.Env.EnvConfiguration (fromEnvVariables)
import qualified Data.ByteString.Char8 as LBS
import ValueObject.Entity.OpenAIConfiguration (openAIAPIKey, OpenAIConfiguration (openAIOrganization))
import qualified Data.Text.Encoding as TE
import ValueObject.Entity.Error (Error(_message))
import ValueObject.Entity.ErrorResponse (ErrorResponse)
import Data.Aeson (eitherDecode)
import Network.HTTP.Types (statusIsSuccessful, statusIsServerError)
import qualified ValueObject.Entity.ErrorResponse as ErrorResponse
import Network.HTTP.Types.Status (statusIsClientError)


retrieveModelURL :: Text
retrieveModelURL = openAIBaseURL <> "models/"

help :: Text
help = "Retrieves a model instance, providing basic information about the model such as the owner and permissioning."

-- GET implementation to retrieve a model calling the operationId: retrieveModel
retrieveModel :: Text -> IO (Either Text Model)
retrieveModel modelId 
    | T.null modelId = return $ Left "Model id is required"
    | otherwise = do
    tlsManager <- newTlsManager
    request <- parseRequest $ T.unpack $ retrieveModelURL <> modelId
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
                    let eitherModel = eitherDecode $ responseBody response :: Either String Model
                    case eitherModel of
                        Right model -> return $ Right model
                        Left err -> return $ Left ("Error decoding response body to Model" <> T.pack err)
                status | statusIsClientError status ->
                    return $ do
                        case eitherError of
                            Right errorResponse -> do
                                let err = ErrorResponse.error errorResponse
                                Left $ "Client error when calling retrieveModel: " <> _message err
                            Left err -> Left $ "Client error when calling retrieveModel: " <> T.pack err
                status | statusIsServerError status ->
                    return $ do
                        case eitherError of
                            Right errorResponse -> do
                                let err = ErrorResponse.error errorResponse
                                Left $ "Server error when calling retrieveModel: " <> _message err
                            Left err -> Left $ "Server error when calling retrieveModel: " <> T.pack err
                _ -> 
                    return $ Left $ "Unknown error when calling listModels: " <> body
