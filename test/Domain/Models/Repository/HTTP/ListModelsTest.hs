module Domain.Models.Repository.HTTP.ListModelsTest where
import Test.Tasty (TestTree, testGroup)
import ValueObject.Repository.Env.EnvConfigurationTest (resetEnv, defaultEnv)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)
import Domain.Models.Repository.HTTP.ListModels (listModels)
import qualified Data.Text as T

allTests :: TestTree
allTests = testGroup "ListModelsTest.hs"
    [
        testListModels
    ]

testListModels :: TestTree
testListModels = testGroup "Test suite for listModels function"
    [
        testCase "Test listModels with no OpenAI API Key" $ do
            resetEnv
            let expected = T.pack "OPENAI_API_KEY environment variable not found"
            eitherModelList <- listModels
            case eitherModelList of
                Left error -> assertEqual "Error Message" expected error
                Right _ -> assertFailure "Expected Left, but got Right"
    ,   testCase "Test listModels with a invalid OpenAI API Key" $ do
            resetEnv
            defaultEnv
             
    ]