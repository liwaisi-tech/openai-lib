{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ValueObject.Entity.ErrorTest where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import ValueObject.Entity.Error (Error(..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import Data.String.Here (here)

jsonError :: String
jsonError = [here|
{
    "message": "Incorrect API key provided: sk-ZMClO**************************************wUbB. You can find your API key at https://platform.openai.com/account/api-keys.",
    "type": "invalid_request_error",
    "param": null,
    "code": "invalid_api_key"
  }
|]

allTests :: TestTree
allTests = testGroup "ErrorTest.hs"
    [ testJSONDecode
    ]

testJSONDecode :: TestTree
testJSONDecode = testGroup "JSON Decode test for Error"
    [
        testCase "Test JSON decode with a valid JSON" $ do
            let expected = Error {
                _message = "Incorrect API key provided: sk-ZMClO**************************************wUbB. You can find your API key at https://platform.openai.com/account/api-keys.",
                _type = "invalid_request_error",
                _param = Nothing,
                _code = Just "invalid_api_key"
            }
            let actual = eitherDecode $ LBS.pack jsonError
            assertEqual "Should be equal" (Right expected) actual
    ]
