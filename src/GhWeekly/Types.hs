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

    , CallTime
    , GithubState(..)
    , ghCalls
    , ghRPM
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
import qualified Data.Sequence            as S
import qualified Data.Text                as T
import           Data.Time


type OauthToken = T.Text
type GhAuth     = OauthToken
type Param      = (T.Text, T.Text)
type Sha        = T.Text
type CallTime   = UTCTime

data GhWeekly
        = GhWeekly
        { _ghwUser       :: !T.Text
        , _ghwOrgs       :: !(Maybe T.Text)
        , _ghwDays       :: !Int
        , _ghwOauthToken :: !OauthToken
        , _ghwSince      :: !(Maybe UTCTime)
        , _ghwVerbose    :: !Bool
        } deriving (Show)
makeLenses ''GhWeekly

data GithubState
        = GHState
        { _ghCalls   :: !(S.Seq CallTime)
        , _ghRPM     :: !Int
        , _ghVerbose :: !Bool
        }
makeLenses ''GithubState

-- Requests/minute here is the authenticated search rate limit.
defaultState :: GithubState
defaultState = GHState S.empty 20 False

newtype Github a
    = Github
    { unGithub :: EitherT SomeException (RWST GhAuth (Sum Int) GithubState IO) a
    }
    deriving (Functor, Applicative, Monad)

instance MonadIO Github where
    liftIO = Github . EitherT . liftIO . try

instance MonadState GithubState Github where
    get = Github . EitherT . fmap Right $ get
    put = Github . EitherT . fmap Right . put
    state f = do
        (a, s) <- f <$> get
        put s
        return a

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

runGithub :: GhAuth -> Bool -> Github a -> IO (Either SomeException a, Int)
runGithub auth verbose gh =
    fmap getSum <$> evalRWST (runEitherT $ unGithub gh)
                             auth
                             (defaultState & ghVerbose .~ verbose)

hoistEitherGH :: Either SomeException a -> Github a
hoistEitherGH = Github . EitherT . return

hoistEitherGH' :: Either String a -> Github a
hoistEitherGH' = hoistEitherGH . fmapL (toException . ErrorCall)
