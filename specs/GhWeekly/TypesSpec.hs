{-# LANGUAGE TupleSections #-}


module GhWeekly.TypesSpec where


import           Control.Arrow
import           Data.Aeson
import qualified Data.ByteString as B
import           Data.Either

import           Test.Hspec

import           GhWeekly.Types


type Decode a  = Either String a
type Success a = Decode a -> Bool

fixture :: String -> String
fixture = ("./specs/fixtures/" ++)

canParseBytes :: (Show a, FromJSON a)
              => B.ByteString -> (Decode a, Expectation)
canParseBytes = (id &&& (`shouldSatisfy` (isRight :: Success a)))
              . eitherDecodeStrict'

canParse :: (Show a, FromJSON a) => FilePath -> IO (Decode a, Expectation)
canParse = fmap canParseBytes . B.readFile . fixture

spec :: Spec
spec =
    describe "Repo" $ do
        it "should decode the API file." $
            (canParse "repo-list.json" :: IO (Decode [Repo], Expectation)) >>= snd
        it "should decode the list all public repositories example." $
            (canParse "repo-all.json" :: IO (Decode [Repo], Expectation)) >>= snd
        it "should decode the get repo output." $
            (canParse "repo-get.json" :: IO (Decode Repo, Expectation)) >>= snd
