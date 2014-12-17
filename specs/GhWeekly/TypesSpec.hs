

module GhWeekly.TypesSpec where


import           Data.Aeson
import qualified Data.ByteString as B
import           Data.Either

import           Test.Hspec

import           GhWeekly.Types


spec :: Spec
spec =
    describe "Repo" $ do
        bytes <- runIO $ B.readFile "./specs/fixtures/repo-output.json"

        it "should successfully decode the API file." $
            eitherDecodeStrict' bytes `shouldSatisfy`
                (isRight :: Either String [Repo] -> Bool)
