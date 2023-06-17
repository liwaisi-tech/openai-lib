{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Domain.Models.Usecases.Client as ModelClient
import qualified Data.Text.IO as TIO
import Domain.Models.Entity.Model (Model(id))
import qualified Domain.Models.Entity.Model as M
import qualified Data.Text as T

main :: IO ()
main = do
  eitherModelList <- ModelClient.listModels
  case eitherModelList of
    Left error -> print error
    Right modelList -> showModelList modelList
  eitherModel <- ModelClient.retrieveModel "gpt-3.5-turbo-16k"
  case eitherModel of
    Left error -> print error
    Right model -> TIO.putStrLn $ T.pack $ show model

showModelList :: [Model] -> IO()
showModelLits [] = return ()
showModelList [x] = TIO.putStrLn $ M.id x
showModelList (x:xs) = do
  TIO.putStrLn $ M.id x
  showModelList xs