{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Network
    ( githubUrl
    , github
    , getAllUserRepos
    ) where


import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Network.Wreq

import           GhWeekly.Types


githubUrl :: String
githubUrl = "https://api.github.com"

github :: FromJSON a => String -> [Param] -> Github a
github path ps = do
    token <- asks (encodeUtf8 . mappend "token ")
    let opts = defaults & header "Accept" .~ ["application/vnd.github.v3+json"]
                        & header "Authorization" .~ [token]
                        & params .~ ps
    hoistEitherGH
        .   fmapL (SomeException . ErrorCall)
        .   eitherDecode
        .   view responseBody
        =<< liftIO (getWith opts fullPath)
    where
        fullPath = githubUrl ++ path

getAllUserRepos :: T.Text -> Github [Object]
getAllUserRepos user = github path [("type", "all")]
    where
        path = T.unpack $ mconcat ["/users/", user, "/repos"]
