module Domain.Models.Usecases.Client where
import Domain.Models.Entity.Model (Model)
import Domain.Models.Entity.ModelList (ModelList(_data))
import Data.Text (Text)
import qualified Domain.Models.Repository.HTTP.ListModels as HTTP

listModels :: IO (Either Text [Model])
listModels = do
    listModelsResponse <- HTTP.listModels
    case listModelsResponse of
        Left error -> return $ Left error
        Right modelList -> return $ Right $ _data modelList