{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module GhWeekly.Types
    ( GhWeekly(..)
    , ghwUser
    , ghwOrgs

    , User(..)
    , userLogin
    , userID
    , userAvatarUrl
    , userGravatarId
    , userUrl
    , userHtmlUrl
    , userFollowersUrl
    , userFollowingUrl
    , userGistsUrl
    , userStarredUrl
    , userSubscriptionsUrl
    , userOrganizationsUrl
    , userReposUrl
    , userEventsUrl
    , userReceivedEventsUrl
    , userType
    , userSiteAdmin

    , RepoPerms(..)
    , permsAdmin
    , permsPush
    , permsPull

    , Repo(..)
    , repoID
    , repoOwner
    , repoName
    , repoFullName
    , repoDescription
    , repoPrivate
    , repoFork
    , repoUrl
    , repoHtmlUrl
    , repoCloneUrl
    , repoGitUrl
    , repoSshUrl
    , repoSvnUrl
    , repoMirrorUrl
    , repoHomepage
    , repoLanguage
    , repoForksCount
    , repoStargazersCount
    , repoWatchersCount
    , repoSize
    , repoDefaultBranch
    , repoOpenIssuesCount
    , repoHasIssues
    , repoHasWiki
    , repoHasPages
    , repoHasDownloads
    , repoPushedAt
    , repoCreatedAt
    , repoUpdatedAt
    , repoPermissions

    , Event
    , Commit

    , RepoReport(..)
    , rrRepo
    , rrIssueEvents
    , rrCommits

    , UserReport(..)
    , reportUser
    , reportRepo
    ) where


import           Control.Lens
import           Data.Aeson.TH
import qualified Data.List      as L
import           Data.Text
import           Data.Time

import           GhWeekly.Utils


data GhWeekly
        = GhWeekly
        { _ghwUser :: !Text
        , _ghwOrgs :: !(Maybe Text)
        } deriving (Show)
makeLenses ''GhWeekly

data User
        = User
        { _userLogin             :: !Text
        , _userID                :: !Int
        , _userAvatarUrl         :: !Text
        , _userGravatarId        :: !Text
        , _userUrl               :: !Text
        , _userHtmlUrl           :: !Text
        , _userFollowersUrl      :: !Text
        , _userFollowingUrl      :: !Text
        , _userGistsUrl          :: !Text
        , _userStarredUrl        :: !Text
        , _userSubscriptionsUrl  :: !Text
        , _userOrganizationsUrl  :: !Text
        , _userReposUrl          :: !Text
        , _userEventsUrl         :: !Text
        , _userReceivedEventsUrl :: !Text
        , _userType              :: !Text
        , _userSiteAdmin         :: !Bool
        }
        deriving (Show)
makeLenses ''User
$(deriveJSON defaultOptions { fieldLabelModifier = decamel . L.drop 5
                            } ''User)

data RepoPerms
        = RepoPerms
        { _permsAdmin :: !Bool
        , _permsPush  :: !Bool
        , _permsPull  :: !Bool
        }
        deriving (Show)
makeLenses ''RepoPerms
$(deriveJSON defaultOptions { fieldLabelModifier = decamel . L.drop 6
                            } ''RepoPerms)

data Repo
        = Repo
        { _repoID              :: !Int
        , _repoOwner           :: !User
        , _repoName            :: !Text
        , _repoFullName        :: !Text
        , _repoDescription     :: !Text
        , _repoPrivate         :: !Bool
        , _repoFork            :: !Bool
        , _repoUrl             :: !Text
        , _repoHtmlUrl         :: !Text
        , _repoCloneUrl        :: !Text
        , _repoGitUrl          :: !Text
        , _repoSshUrl          :: !Text
        , _repoSvnUrl          :: !Text
        , _repoMirrorUrl       :: !Text
        , _repoHomepage        :: !Text
        , _repoLanguage        :: !(Maybe Text)
        , _repoForksCount      :: !Int
        , _repoStargazersCount :: !Int
        , _repoWatchersCount   :: !Int
        , _repoSize            :: !Int
        , _repoDefaultBranch   :: !Text
        , _repoOpenIssuesCount :: !Int
        , _repoHasIssues       :: !Bool
        , _repoHasWiki         :: !Bool
        , _repoHasPages        :: !Bool
        , _repoHasDownloads    :: !Bool
        , _repoPushedAt        :: !UTCTime
        , _repoCreatedAt       :: !UTCTime
        , _repoUpdatedAt       :: !UTCTime
        , _repoPermissions     :: !RepoPerms
        }
        deriving (Show)
makeLenses ''Repo
$(deriveJSON defaultOptions { fieldLabelModifier = decamel . L.drop 5
                            } ''Repo)

data Event
data Commit

data RepoReport
        = RepoReport
        { _rrRepo        :: !Repo
        , _rrIssueEvents :: ![Event]
        , _rrCommits     :: ![Commit]
        }
makeLenses ''RepoReport

data UserReport
        = UserReport
        { _reportUser :: !User
        , _reportRepo :: !RepoReport
        }
makeLenses ''UserReport
