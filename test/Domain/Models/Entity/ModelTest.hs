{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Models.Entity.ModelTest where
import Test.Tasty (TestTree, testGroup)
import Data.String.Here (here)
import Domain.Models.Entity.Model (Model(..))
import Test.Tasty.HUnit (testCase, assertEqual)
import qualified Domain.Models.Entity.Model as ModelEntity
import qualified Domain.Models.Entity.ModelPermission as PermissionEntity
import Domain.Models.Entity.ModelPermission (ModelPermission(..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)


modelJson :: String
modelJson = [here|
{
  "id": "gpt-3.5-turbo-16k",
  "object": "model",
  "created": 1683758102,
  "owned_by": "openai-internal",
  "permission": [
    {
      "id": "modelperm-LMK1z45vFJF9tVUvKb3pZfMG",
      "object": "model_permission",
      "created": 1686799823,
      "allow_create_engine": false,
      "allow_sampling": true,
      "allow_logprobs": true,
      "allow_search_indices": false,
      "allow_view": true,
      "allow_fine_tuning": false,
      "organization": "*",
      "group": null,
      "is_blocking": false
    }
  ],
  "root": "gpt-3.5-turbo-16k",
  "parent": null
}
|]

allTests :: TestTree
allTests = testGroup "ModelTest.hs"
    [ testJSONDecode
    ]


testJSONDecode :: TestTree
testJSONDecode = testGroup "Model JSON decode tests"
    [
        testCase "Test JSON decode with a valid JSON" $ do
        let expected = Model {
            ModelEntity.id = "gpt-3.5-turbo-16k",
            ModelEntity.object = "model",
            ModelEntity.created = 1683758102,
            owned_by = "openai-internal",
            permission = [
                ModelPermission {
                    PermissionEntity.id = "modelperm-LMK1z45vFJF9tVUvKb3pZfMG",
                    PermissionEntity.object = "model_permission",
                    PermissionEntity.created = 1686799823,
                    allow_create_engine = False,
                    allow_sampling = True,
                    allow_logprobs = True,
                    allow_search_indices = False,
                    allow_view = True,
                    allow_fine_tuning = False,
                    organization = "*",
                    group = Nothing,
                    is_blocking = False
                }
            ],
            root = "gpt-3.5-turbo-16k",
            parent = Nothing
        }
        let actual = eitherDecode (LBS.pack modelJson)
        assertEqual "Should be equal" (Right expected) actual
    ]