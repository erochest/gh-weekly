{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Network
    ( githubUrl
    , github
    , github'
    , gh
    , getUser
    , getUserOrgs
    , getOrg
    , getAllUserRepos
    , getOrgRepos
    , getRepoCommitsFor
    ) where


import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Time
import           Network.Wreq
import           System.Locale

import           GhWeekly.Lens
import           GhWeekly.Types


githubUrl :: String
githubUrl = "https://api.github.com"

github :: FromJSON a => String -> [Param] -> Github a
github p = github' (githubUrl ++ p)

github' :: FromJSON a => String -> [Param] -> Github a
github' fullPath ps = do
    token <- asks (encodeUtf8 . mappend "token ")
    let opts = defaults & header "Accept" .~ ["application/vnd.github.v3+json"]
                        & header "Authorization" .~ [token]
                        & params .~ ps
    liftIO . putStrLn $ "github: " ++ fullPath
    hoistEitherGH
        .   fmapL (SomeException . ErrorCall)
        .   eitherDecode
        .   view responseBody
        =<< liftIO (getWith opts fullPath)

gh :: FromJSON a => [T.Text] -> [Param] -> Github a
gh ps = github (path ps)

path :: [T.Text] -> String
path = T.unpack . mconcat

getUser :: T.Text -> Github Value
getUser user = gh ["/users/", user] []

getUserOrgs :: T.Text -> Github [Value]
getUserOrgs user =   mapM ((`github'` []) . T.unpack)
                 .   mapMaybe (preview (url . _String))
                 =<< (gh ["/users/", user, "/orgs"] [] :: Github [Value])

getOrg :: T.Text -> Github Value
getOrg org = gh ["/orgs/", org] []

getAllUserRepos :: T.Text -> Github [Value]
getAllUserRepos user = gh ["/users/", user, "/repos"] [("type", "all")]

getOrgRepos :: T.Text -> Github [Value]
getOrgRepos org = gh ["/orgs/", org, "/repos"] []

getRepoCommitsFor :: T.Text -> T.Text -> UTCTime -> Github [Value]
getRepoCommitsFor fullRepoName user since =
    gh ["/repos/", fullRepoName, "/commits"] [ ("author", user)
                                             , ("since",  since')
                                             ]
    where
        since' = T.pack $ formatTime defaultTimeLocale "%FT%TZ" since
