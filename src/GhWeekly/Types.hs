{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module GhWeekly.Types
    ( GhWeekly(..)
    , ghwUser
    , ghwOrgs

    , RepoReport(..)
    , rrRepo
    , rrIssueEvents
    , rrCommits

    , UserReport(..)
    , reportUser
    , reportRepo
    ) where


import           Control.Lens
import           Data.Aeson
import qualified Data.Text    as T


data GhWeekly
        = GhWeekly
        { _ghwUser :: !T.Text
        , _ghwOrgs :: !(Maybe T.Text)
        } deriving (Show)
makeLenses ''GhWeekly

data RepoReport
        = RepoReport
        { _rrRepo        :: !Object
        , _rrIssueEvents :: ![Object]
        , _rrCommits     :: ![Object]
        }
makeLenses ''RepoReport

data UserReport
        = UserReport
        { _reportUser :: !Object
        , _reportRepo :: !RepoReport
        }
makeLenses ''UserReport
