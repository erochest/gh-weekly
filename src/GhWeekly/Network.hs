{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module GhWeekly.Network
    ( githubUrl
    , github
    , github'
    , gh
    , throttle
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
import           Control.Concurrent
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import qualified Control.Monad.RWS.Strict as RWS
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8    as C
import           Data.Foldable            hiding (concat)
import           Data.Monoid
import qualified Data.Sequence            as S
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.Format         as F
import           Data.Text.Lazy           (toStrict)
import           Data.Time
import           Data.Time.Clock.POSIX
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
    GHConfig auth verbose <- ask
    let token = encodeUtf8 $ "token " <> auth
        opts = defaults & header "Accept" .~ ["application/vnd.github.v3+json"]
                        & header "Authorization" .~ [token]
    go verbose opts (Just fullUrl) ps

    where
        decodeResponse = hoistEitherGH'
                       . eitherDecode
                       . (^. responseBody)

        next r = r ^? responseLink "rel" "next" . linkURL . to C.unpack

        go _ _ Nothing _       = return []
        go v opts (Just u) ps' = do
            when v $ liftIO $
                F.print "{}\t{}\n" . (, u) =<< getCurrentTime
            r <- liftIO $ getWith (opts & params .~ ps') u
            throttle r >> countCall
            (:) <$> decodeResponse r <*> go v opts (next r) []

gh :: FromJSON a => [T.Text] -> [Param] -> Github [a]
gh ps = github (path ps)

_Int :: Prism' C.ByteString Int
_Int = prism' fint tint
    where
        fint   = C.pack . show
        tint s = case C.readInt s of
                     Just (n, "") -> Just n
                     Just _       -> Nothing
                     Nothing      -> Nothing

-- throttle by waiting 0.5 of the time
-- or by waiting the length to the reset time / remaining count
throttle :: Response a -> Github ()
throttle r = do
    now   <- liftIO getCurrentTime
    reset <- fmap (posixSecondsToUTCTime . fromIntegral)
          .  hoistMaybeGH "Invalid X-RateLimit-Reset value."
          $  r ^? responseHeader "X-RateLimit-Reset" . _Int
    let delay = floor $ reset `diffUTCTime` now
    when (reset > now && delay > 0) $ do
        verbose <- asks _ghcVerbose
        when verbose . liftIO .
            F.print "DELAYING {} ....\n" $ Only delay
        liftIO . threadDelay $ delay * 1000

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
    gh ["/search/issues"] [("q", toStrict query)]
    where
        since' = T.pack $ formatTime defaultTimeLocale "%FT%TZ" since
        query  = F.format "author:{} updated:>{} repo:{}" (username, since', repo)
