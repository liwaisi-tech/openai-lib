{-# LANGUAGE OverloadedStrings #-}
module ValueObject.Repository.Env.EnvConfigurationTest where

import Test.Tasty
import Test.Tasty.HUnit
import ValueObject.Repository.Env.EnvConfiguration (fromEnvVariables)
import ValueObject.Entity.OpenAIConfiguration (OpenAIConfiguration(..))
import System.Environment (setEnv)

allTests :: TestTree
allTests = testGroup "EnvConfigurationTest.hs"
    [ testEnvOpenAIAPIKey
    ]

testEnvOpenAIAPIKey :: TestTree
testEnvOpenAIAPIKey = testGroup "EnvConfiguration tests"
    [ testCase "Empty API key calling fromEnvVariables" $ do
        resetEnv
        eitherConfiguration <- fromEnvVariables
        case eitherConfiguration of
            Left err -> assertEqual "Error Message" "OPENAI_API_KEY environment variable not found" err
            Right _ -> assertFailure "Expected Left, but got Right"
    , testCase "Valid API Key calling fromEnvVariables" $ do
        resetEnv
        setEnv "OPENAI_API_KEY" "0123456789ABCDEF"
        eitherConfiguration <- fromEnvVariables
        case eitherConfiguration of
            Left err -> do
                assertFailure "Expected Right, but got Left" 
            Right configuration -> do 
                assertEqual "API Key" "0123456789ABCDEF" (openAIAPIKey configuration)
                assertEqual "Organization" "" (openAIOrganization configuration)
    , testCase "Valid API Key and Organization calling fromEnvVariables" $ do
        resetEnv
        defaultEnv
        eitherConfiguration <- fromEnvVariables
        case eitherConfiguration of
            Left err -> do
                assertFailure "Expected Right, but got Left" 
            Right configuration -> do 
                assertEqual "API Key" "0123456789ABCDEF" (openAIAPIKey configuration)
                assertEqual "Organization" "Organization" (openAIOrganization configuration)
    ]

resetEnv :: IO ()
resetEnv = do
    setEnv "OPENAI_API_KEY" ""
    setEnv "OPENAI_ORGANIZATION" ""

defaultEnv :: IO ()
defaultEnv = do
    setEnv "OPENAI_API_KEY" "0123456789ABCDEF"
    setEnv "OPENAI_ORGANIZATION" "Organization"