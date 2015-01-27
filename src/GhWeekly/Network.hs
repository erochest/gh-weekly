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
    , getBranchCommits
    , getBranches
    , getAllCommits
    , getCommit
    , getIssuesInvolving
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as C
import           Data.Foldable         hiding (concat)
import           Data.Monoid
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Data.Time
import           Network.Wreq
import           System.Locale

import           GhWeekly.Lens
import           GhWeekly.Types
import           GhWeekly.Utils


githubUrl :: String
githubUrl = "https://api.github.com"

github :: FromJSON a => String -> [Param] -> Github [a]
github p = github' (githubUrl ++ p)

github' :: FromJSON a => String -> [Param] -> Github [a]
github' fullUrl ps = do
    token <- asks (encodeUtf8 . mappend "token ")
    let opts = defaults & header "Accept" .~ ["application/vnd.github.v3+json"]
                        & header "Authorization" .~ [token]
    go opts (Just fullUrl) ps
    where

        decodeResponse = hoistEitherGH'
                       . eitherDecode
                       . (^. responseBody)

        next r = r ^? responseLink "rel" "next" . linkURL . to C.unpack

        go _ Nothing _      = return []
        go opts (Just u) ps' = do
            -- liftIO . putStrLn $  "github: " ++ u
            countCall
            r <- liftIO $ getWith (opts & params .~ ps') u
            (:) <$> decodeResponse r <*> go opts (next r) []

gh :: FromJSON a => [T.Text] -> [Param] -> Github [a]
gh ps = github (path ps)

path :: [T.Text] -> String
path = T.unpack . mconcat

orNull :: [Value] -> Value
orNull []    = Null
orNull (v:_) = v

getUser :: T.Text -> Github Value
getUser user = orNull <$> gh ["/users/", user] []

getUserOrgs :: T.Text -> Github [Value]
getUserOrgs user =   fmap (toList . mconcat)
                 .   mapM ((`github'` []) . T.unpack)
                 .   mapMaybe (preview (url . _String))
                 .   toList
                 .   mconcat
                 =<< (gh ["/users/", user, "/orgs"] [] :: Github [[Value]])

getOrg :: T.Text -> Github Value
getOrg org = orNull <$> gh ["/orgs/", org] []

getAllUserRepos :: T.Text -> Github [Value]
getAllUserRepos user =
    concat <$> gh ["/users/", user, "/repos"] [("type", "all")]

getOrgRepos :: T.Text -> Github [Value]
getOrgRepos org = concat <$> gh ["/orgs/", org, "/repos"] []

getRepoCommitsFor :: T.Text -> T.Text -> UTCTime -> Github [Value]
getRepoCommitsFor fullRepoName user since =
        concat
    <$> gh ["/repos/", fullRepoName, "/commits"] [ ("author", user)
                                                 , ("since",  since')
                                                 ]
    where
        since' = T.pack $ formatTime defaultTimeLocale "%FT%TZ" since

getBranchCommits :: T.Text -> T.Text -> UTCTime -> T.Text -> Github [Value]
getBranchCommits fullRepoName user since branch =
        concat
    <$> gh ["/repos/", fullRepoName, "/commits"] [ ("author", user)
                                                 , ("since", since')
                                                 , ("sha", branch)
                                                 ]
    where
        since' = T.pack $ formatTime defaultTimeLocale "%FT%TZ" since

getBranches :: T.Text -> Github [T.Text]
getBranches fullRepoName =
        mapMaybe (preview (name . _String)) . concat
    <$> (gh ["/repos/", fullRepoName, "/branches"] [] :: Github [[Value]])

getAllCommits :: T.Text -> T.Text -> UTCTime -> Github [Value]
getAllCommits fullRepoName user since =
    fmap concat . mapM (getBranchCommits fullRepoName user since)
        =<< getBranches fullRepoName

getCommit :: T.Text -> Sha -> Github Value
getCommit fullRepoName sha =
    fromMaybe Null . headZ <$> gh ["/repos/", fullRepoName, "/commits/", sha] []

getIssuesInvolving :: T.Text -> UTCTime -> T.Text -> Github [Value]
getIssuesInvolving username since repo =
    gh ["/search/issues"] [("q",  "author:"    <> username
                               <> " updated:>" <> since'
                               <> " repo:"     <> repo
                               )]
    where
        since' = T.pack $ formatTime defaultTimeLocale "%FT%TZ" since
