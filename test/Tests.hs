module Main (main) where
import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified ValueObject.Entity.OpenAIConfigurationTest as OpenAIConfigurationTest

main :: IO ()
main = do
    putStrLn "Test suite for openai-lib module."
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [
        OpenAIConfigurationTest.allTests
    ]
