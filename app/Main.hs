module Main where
import qualified Domain.Models.Usecases.Client as ModelClient

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  eitherModelList <- ModelClient.listModels
  case eitherModelList of
    Left error -> print error
    Right modelList -> print modelList
