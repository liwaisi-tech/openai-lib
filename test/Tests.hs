module Main (main) where
import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified ValueObject.Entity.OpenAIConfigurationTest as OpenAIConfigurationTest
import qualified ValueObject.Repository.Env.EnvConfigurationTest as EnvConfigurationTest
import qualified Domain.Models.Entity.ModelPermissionTest as ModelPermissionTest
import qualified Domain.Models.Repository.HTTP.ListModelsTest as ListModelsTest
import qualified ValueObject.Entity.ErrorTest as ErrorTest

main :: IO ()
main = do
    putStrLn "Test suite for openai-lib module."
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [
        OpenAIConfigurationTest.allTests
      , EnvConfigurationTest.allTests
      , ModelPermissionTest.allTests
      , ListModelsTest.allTests
      , ErrorTest.allTests
    ]
