{-# LANGUAGE OverloadedStrings #-}
module ValueObject.Entity.ErrorTest where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import ValueObject.Entity.Error (Error(..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

allTests :: TestTree
allTests = testGroup "ErrorTest.hs"
    [ testJSONDecode
    ]

testJSONDecode :: TestTree
testJSONDecode = testGroup "JSON Decode test for Error"
    [
        testCase "Test JSON decode with a valid JSON" $ do
            let json = "{\n  \"error\": {\n    \"message\": \"Incorrect API key provided: sk-ZMClO**************************************wUbB. You can find your API key at https://platform.openai.com/account/api-keys.\",\n    \"type\": \"invalid_request_error\",\n    \"param\": null,\n    \"code\": \"invalid_api_key\"\n  }\n}"
            let expected = Error {
                _message = "Incorrect API key provided: sk-ZMClO**************************************wUbB. You can find your API key at https://platform.openai.com/account/api-keys.",
                _type = "invalid_request_error",
                _param = Nothing,
                _code = Just "invalid_api_key"
            }
            let actual = eitherDecode $ LBS.pack json
            assertEqual "Should be equal" (Right expected) actual
    ]
