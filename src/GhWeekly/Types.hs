{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module GhWeekly.Types
    ( GhWeekly(..)
    , ghwUser
    , ghwOrgs
    , ghwDays
    , ghwOauthToken
    , ghwSince

    , GithubState
    , defaultState

    , GhAuth
    , Github
    , runGithub
    , hoistEitherGH
    , hoistEitherGH'
    , Param

    , Sha
    ) where


import           Control.Applicative
import           Control.Error            hiding (tryJust)
import           Control.Exception
import           Control.Lens
import           Control.Monad.RWS.Strict
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                as T
import           Data.Time


type OauthToken = T.Text
type GhAuth     = OauthToken
type Param      = (T.Text, T.Text)
type Sha        = T.Text

data GhWeekly
        = GhWeekly
        { _ghwUser       :: !T.Text
        , _ghwOrgs       :: !(Maybe T.Text)
        , _ghwDays       :: !Int
        , _ghwOauthToken :: !OauthToken
        , _ghwSince      :: !(Maybe UTCTime)
        } deriving (Show)
makeLenses ''GhWeekly

data GithubState
        = GHState

defaultState :: GithubState
defaultState = GHState

newtype Github a
    = Github
    { unGithub :: EitherT SomeException (RWST GhAuth (Sum Int) GithubState IO) a
    }
    deriving (Functor, Applicative, Monad)

instance MonadIO Github where
    liftIO = Github . EitherT . liftIO . try

instance MonadReader GhAuth Github where
    ask     = Github ask
    local f = Github . local f . unGithub

instance MonadWriter (Sum Int) Github where
    tell   = Github . lift . tell
    listen = Github . listen . unGithub
    pass m = do
        (a, fw) <- m
        ((), w) <- listen $ return ()
        writer (a, fw w)

runGithub :: GhAuth -> Github a -> IO (Either SomeException a, Int)
runGithub auth gh =
    fmap getSum <$> evalRWST (runEitherT $ unGithub gh) auth defaultState

hoistEitherGH :: Either SomeException a -> Github a
hoistEitherGH = Github . EitherT . return

hoistEitherGH' :: Either String a -> Github a
hoistEitherGH' = hoistEitherGH . fmapL (toException . ErrorCall)
