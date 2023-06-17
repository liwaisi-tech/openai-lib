module Domain.Models.Usecases.Client where
import Domain.Models.Entity.Model (Model)
import Domain.Models.Entity.ListModelsResponse (ListModelsResponse(_data))
import Data.Text (Text)
import qualified Domain.Models.Repository.HTTP.ListModels as LMClient
import qualified Domain.Models.Repository.HTTP.RetrieveModel as RMClient

listModels :: IO (Either Text [Model])
listModels = do
    listModelsResponse <- LMClient.listModels
    case listModelsResponse of
        Left error -> return $ Left error
        Right modelList -> return $ Right modelList

retrieveModel :: Text -> IO (Either Text Model)
retrieveModel modelId = do
    retrieveModelResponse <- RMClient.retrieveModel modelId
    case retrieveModelResponse of
        Left error -> return $ Left error
        Right model -> return $ Right model