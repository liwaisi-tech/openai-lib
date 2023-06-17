{-# LANGUAGE OverloadedStrings #-}
module ValueObject.Entity.OpenAIConfigurationTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import ValueObject.Entity.OpenAIConfiguration (newConfiguration, OpenAIConfiguration (openAIAPIKey, openAIOrganization))

allTests :: TestTree
allTests = testGroup "OpenAIConfigurationTest.hs"
    [ testOpenAIAPIKey
    , testOpenAIOrganization
    ]

testOpenAIAPIKey :: TestTree
testOpenAIAPIKey = testGroup "Test OpenAIConfiguration entity created."
    [ testCase "OpenAIConfiguration empty APIKey" $
        let config = newConfiguration "" ""
        in openAIAPIKey config @?= ""
    , testCase "OpenAIConfiguration valid APIKey" $
        let config = newConfiguration "APIKey" ""
        in openAIAPIKey config @?= "APIKey"
    ]

testOpenAIOrganization :: TestTree
testOpenAIOrganization = testGroup "Test OpenAIConfiguration entity created."
    [ testCase "OpenAIConfiguration empty Organization" $
        let config = newConfiguration "" ""
        in openAIOrganization config @?= ""
    
    , testCase "OpenAIConfiguration valid Organization" $
        let config = newConfiguration "" "Organization"
        in openAIOrganization config @?= "Organization"
    ]