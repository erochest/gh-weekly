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
type Parsed a  = IO (Decode a, Expectation)

fixture :: String -> String
fixture = ("./specs/fixtures/" ++)

canParseBytes :: (Show a, FromJSON a)
              => B.ByteString -> (Decode a, Expectation)
canParseBytes = (id &&& (`shouldSatisfy` (isRight :: Success a)))
              . eitherDecodeStrict'

canParse :: (Show a, FromJSON a) => FilePath -> IO (Decode a, Expectation)
canParse = fmap canParseBytes . B.readFile . fixture

spec :: Spec
spec = do
    describe "Repo" $ do
        it "should decode the list file." $
            (canParse "repo-list.json" :: Parsed [Repo]) >>= snd
        it "should decode the list all public repositories example." $
            (canParse "repo-all.json" :: Parsed [Repo]) >>= snd
        it "should decode the get repo output." $
            (canParse "repo-get.json" :: Parsed Repo) >>= snd

    describe "User" $ do
        it "should decode the get file." $
            (canParse "user-get.json" :: Parsed User) >>= snd
        it "should decode the all file." $
            (canParse "user-all.json" :: Parsed [User]) >>= snd

    describe "Organization" $
        it "should decode the get file." $
            (canParse "org-get.json" :: Parsed Organization) >>= snd
