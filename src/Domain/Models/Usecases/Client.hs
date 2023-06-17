module Domain.Models.Usecases.Client where
import Domain.Models.Entity.Model (Model)
import Domain.Models.Entity.ListModelsResponse (ListModelsResponse(_data))
import Data.Text (Text)
import qualified Domain.Models.Repository.HTTP.DeleteModel as DMClient
import qualified Domain.Models.Repository.HTTP.ListModels as LMClient
import qualified Domain.Models.Repository.HTTP.RetrieveModel as RMClient
import ValueObject.Entity.Error (Error)

listModels :: IO (Either Text [Model])
listModels = LMClient.listModels

retrieveModel :: Text -> IO (Either Text Model)
retrieveModel = RMClient.retrieveModel

deleteModel :: Text -> IO (Maybe Error)
deleteModel = DMClient.deleteModel