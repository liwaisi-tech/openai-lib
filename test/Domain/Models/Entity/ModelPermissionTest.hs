{-# LANGUAGE OverloadedStrings #-}
module Domain.Models.Entity.ModelPermissionTest where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertEqual)
import Domain.Models.Entity.ModelPermission (ModelPermission (..))
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (fromRight)

modelPermissionJSON :: String
modelPermissionJSON = "{\n          \"id\": \"modelperm-vXDJwtFoZKeqgwuG1i0eRjFl\",\n          \"object\": \"model_permission\",\n          \"created\": 1686673921,\n          \"allow_create_engine\": false,\n          \"allow_sampling\": true,\n          \"allow_logprobs\": true,\n          \"allow_search_indices\": false,\n          \"allow_view\": true,\n          \"allow_fine_tuning\": false,\n          \"organization\": \"*\",\n          \"group\": null,\n          \"is_blocking\": false\n        }"

modelPermissionListJSON :: String
modelPermissionListJSON = "[" <> modelPermissionJSON <> ", " <> modelPermissionJSON <>"]"

allTests :: TestTree
allTests = testGroup "ModelPermissionTest.hs"
    [ testJSONDecode
    ]

testJSONDecode :: TestTree
testJSONDecode = testGroup "ModelPermission JSON decode tests"
    [
        testCase "Test JSON decode with a valid JSON" $ do
        let expected = ModelPermission
                { Domain.Models.Entity.ModelPermission.id = "modelperm-vXDJwtFoZKeqgwuG1i0eRjFl"
                , object = "model_permission"
                , created = 1686673921
                , allow_create_engine = False
                , allow_sampling = True
                , allow_logprobs = True
                , allow_search_indices = False
                , allow_view = True
                , allow_fine_tuning = False
                , organization = "*"
                , group = Nothing
                , is_blocking = False
                }
        let actual = eitherDecode (LBS.pack modelPermissionJSON)
        assertEqual "Should be equal" (Right expected) actual
    ,   testCase "Test JSON decode with list of ModelPermission" $ do
        let permission = ModelPermission
                { Domain.Models.Entity.ModelPermission.id = "modelperm-vXDJwtFoZKeqgwuG1i0eRjFl"
                , object = "model_permission"
                , created = 1686673921
                , allow_create_engine = False
                , allow_sampling = True
                , allow_logprobs = True
                , allow_search_indices = False
                , allow_view = True
                , allow_fine_tuning = False
                , organization = "*"
                , group = Nothing
                , is_blocking = False
                }
        let expected = [permission, permission]
        let actual = eitherDecode (LBS.pack modelPermissionListJSON)
        assertEqual "Should be equal" (Right expected) actual
        let array = fromRight [] actual
        assertEqual "Length should be equal" (length expected) (length array)
    ]
    