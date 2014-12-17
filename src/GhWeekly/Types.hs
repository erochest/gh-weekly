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

    , Organization(..)
    , orgLogin
    , orgID
    , orgAvatarUrl
    , orgGravatarId
    , orgUrl
    , orgHtmlUrl
    , orgFollowersUrl
    , orgFollowingUrl
    , orgGistsUrl
    , orgStarredUrl
    , orgSubscriptionsUrl
    , orgOrganizationsUrl
    , orgReposUrl
    , orgEventsUrl
    , orgReceivedEventsUrl
    , orgType
    , orgSiteAdmin

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
    , repoParent
    , repoSource

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

data Organization
        = Organization
        { _orgLogin             :: !Text
        , _orgID                :: !Int
        , _orgAvatarUrl         :: !Text
        , _orgGravatarId        :: !Text
        , _orgUrl               :: !Text
        , _orgHtmlUrl           :: !Text
        , _orgFollowersUrl      :: !Text
        , _orgFollowingUrl      :: !Text
        , _orgGistsUrl          :: !Text
        , _orgStarredUrl        :: !Text
        , _orgSubscriptionsUrl  :: !Text
        , _orgOrganizationsUrl  :: !Text
        , _orgReposUrl          :: !Text
        , _orgEventsUrl         :: !Text
        , _orgReceivedEventsUrl :: !Text
        , _orgType              :: !Text
        , _orgSiteAdmin         :: !Bool
        } deriving (Show)
makeLenses ''Organization
$(deriveJSON defaultOptions { fieldLabelModifier = decamel . L.drop 4
                            } ''Organization)

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
        { _repoID               :: !Int
        , _repoOwner            :: !User
        , _repoOrgranization    :: !(Maybe Organization)
        , _repoName             :: !Text
        , _repoFullName         :: !Text
        , _repoDescription      :: !Text
        , _repoPrivate          :: !Bool
        , _repoFork             :: !Bool
        , _repoUrl              :: !Text
        , _repoHtmlUrl          :: !Text
        , _repoCloneUrl         :: !(Maybe Text)
        , _repoGitUrl           :: !(Maybe Text)
        , _repoSshUrl           :: !(Maybe Text)
        , _repoSvnUrl           :: !(Maybe Text)
        , _repoMirrorUrl        :: !(Maybe Text)
        , _repoHomepage         :: !(Maybe Text)
        , _repoLanguage         :: !(Maybe Text)
        , _repoForksCount       :: !(Maybe Int)
        , _repoStargazersCount  :: !(Maybe Int)
        , _repoWatchersCount    :: !(Maybe Int)
        , _repoSubscribersCount :: !(Maybe Int)
        , _repoSize             :: !(Maybe Int)
        , _repoDefaultBranch    :: !(Maybe Text)
        , _repoOpenIssuesCount  :: !(Maybe Int)
        , _repoHasIssues        :: !(Maybe Bool)
        , _repoHasWiki          :: !(Maybe Bool)
        , _repoHasPages         :: !(Maybe Bool)
        , _repoHasDownloads     :: !(Maybe Bool)
        , _repoPushedAt         :: !(Maybe UTCTime)
        , _repoCreatedAt        :: !(Maybe UTCTime)
        , _repoUpdatedAt        :: !(Maybe UTCTime)
        , _repoPermissions      :: !(Maybe RepoPerms)
        , _repoParent           :: !(Maybe Repo)
        , _repoSource           :: !(Maybe Repo)
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
