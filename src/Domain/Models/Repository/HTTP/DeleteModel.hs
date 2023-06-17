{-# LANGUAGE OverloadedStrings #-}
module Domain.Models.Repository.HTTP.DeleteModel where
import Data.Text (Text)
import ValueObject.Repository.HTTP.Constants (openAIBaseURL)
import ValueObject.Entity.Error (Error(..), newError)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import ValueObject.Repository.Env.EnvConfiguration (fromEnvVariables)
import Network.HTTP.Client (Request(requestHeaders, method), parseRequest, httpNoBody, Response (responseBody, responseStatus), httpLbs)
import ValueObject.Entity.OpenAIConfiguration
    ( OpenAIConfiguration(openAIOrganization, openAIAPIKey) )
import qualified Data.ByteString.Char8 as LBS
import Network.HTTP.Types (methodDelete, statusIsSuccessful, statusIsServerError, statusIsClientError)
import qualified ValueObject.Entity.ErrorResponse as ErrorResponse
import qualified Data.Text.Encoding as TE
import Data.Aeson (eitherDecode)

deleteModelUrl :: Text
deleteModelUrl = openAIBaseURL <> "models/"

help :: Text
help = "Delete a fine-tuned model. You must have the Owner role in your organization."

deleteModel :: Text -> IO(Maybe Error)
deleteModel modelId
    | T.null modelId = return $ Just $ newError "Model id is required"
    | otherwise = do
        eitherConfiguration <- fromEnvVariables
        case eitherConfiguration of
            Left error -> return $ Just $ newError error
            Right configuration -> do
                request <- parseRequest $ T.unpack $ deleteModelUrl <> modelId
                tlsManager <- newTlsManager
                let authHeaders = LBS.pack $ "Bearer " <> T.unpack (openAIAPIKey configuration)
                let organizationHeader = LBS.pack $ T.unpack $ openAIOrganization configuration
                let requestWithHeaders = request { requestHeaders = [
                    ("Authorization", authHeaders),
                    ("OpenAI-Organization", organizationHeader)
                ] }
                let request' = requestWithHeaders { method = methodDelete }                
                response <- httpLbs request' tlsManager
                let eitherError = eitherDecode $ responseBody response
                let body = TE.decodeUtf8 $ LBS.toStrict $ responseBody response
                case responseStatus response of
                    status | statusIsSuccessful status -> return Nothing
                    status | statusIsClientError status -> do
                        case eitherError of
                            Right errorResponse -> do
                                let err = ErrorResponse.error errorResponse
                                return $ Just $ newError $ "Client error when calling deleteModel: " <> _message err
                            Left err -> return $ Just $ newError $ "Client error when calling deleteModel: " <> body
                    status | statusIsServerError status -> do
                        case eitherError of
                            Right errorResponse -> do
                                let err = ErrorResponse.error errorResponse
                                return $ Just $ newError $ "Server error when calling deleteModel: " <> _message err
                            Left err -> return $ Just $ newError $ "Server error when calling deleteModel: " <> body
                        return $ Just $ newError "Server error when calling deleteModel"
                    _ -> return $ Just $ newError $ "Unknown error when calling deleteModel: " <> body
                
