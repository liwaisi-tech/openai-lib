# Domain.Models.Usecases.Client

This module provides functions for interacting with the OpenAI API to list, retrieve, and delete models.

### `listModels :: IO (Either Text [Model])`

This function retrieves a lists the currently available models, and provides basic information about each one such as the owner and availability.

#### Example

```haskell
import Domain.Models.Entity.Model (Model)
import Domain.Models.Usecases.Client (listModels)

main :: IO ()
main = do
  result <- listModels
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right models -> mapM_ print models
```

### `retrieveModel :: Text -> IO (Either Text Model)`

This function retrieves a model instance, providing basic information about the model such as the owner and permissioning.

#### Example

```haskell
import Data.Text (pack)
import Domain.Models.Entity.Model (Model)
import Domain.Models.Usecases.Client (retrieveModel)

main :: IO ()
main = do
  let modelId = pack "davinci"
  result <- retrieveModel modelId
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right model -> print model
```

### `deleteModel :: Text -> IO (Maybe Error)`

This function delete a fine-tuned model. You must have the Owner role in your organization.

#### Example

```haskell
import Data.Text (pack)
import Domain.Models.Usecases.Client (deleteModel)

main :: IO ()
main = do
  let modelId = pack "davinci"
  result <- deleteModel modelId
  case result of
    Nothing -> putStrLn "Model deleted successfully"
    Just err -> putStrLn $ "Error: " ++ _message err
```
