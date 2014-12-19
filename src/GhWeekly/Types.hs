{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module GhWeekly.Types
    ( GhWeekly(..)
    , ghwUser
    , ghwOrgs
    , ghwDays
    , ghwOauthToken

    , GhAuth
    , Github
    , runGithub
    , hoistEitherGH
    , Param

    , RepoReport(..)
    , rrRepo
    , rrIssueEvents
    , rrCommits

    , UserReport(..)
    , reportUser
    , reportRepo
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text            as T


type OauthToken = T.Text
type GhAuth     = OauthToken
type Param      = (T.Text, T.Text)

data GhWeekly
        = GhWeekly
        { _ghwUser       :: !T.Text
        , _ghwOrgs       :: !(Maybe T.Text)
        , _ghwDays       :: !Int
        , _ghwOauthToken :: !OauthToken
        } deriving (Show)
makeLenses ''GhWeekly

newtype Github a
    = Github { unGithub :: ReaderT GhAuth (EitherT SomeException IO) a }
    deriving (Functor, Applicative, Monad)

instance MonadIO Github where
    liftIO = Github . liftIO

instance MonadReader GhAuth Github where
    ask     = Github ask
    local f = Github . local f . unGithub

runGithub :: GhAuth -> Github a -> IO (Either SomeException a)
runGithub auth gh = runEitherT $ runReaderT (unGithub gh) auth

hoistEitherGH :: Either SomeException a -> Github a
hoistEitherGH = Github . ReaderT . const . EitherT . return

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
