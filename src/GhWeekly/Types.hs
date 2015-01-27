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

    , GhAuth
    , Github
    , runGithub
    , hoistEitherGH
    , hoistEitherGH'
    , Param

    , Sha
    ) where


import           Control.Applicative
import           Control.Error               hiding (tryJust)
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                   as T
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

newtype Github a
    = Github
    { unGithub :: ReaderT GhAuth (EitherT SomeException (WriterT (Sum Int) IO)) a
    }
    deriving (Functor, Applicative, Monad)

instance MonadIO Github where
    liftIO = Github
           . ReaderT
           . const
           . EitherT
           . WriterT
           . fmap (,mempty)
           . try

instance MonadReader GhAuth Github where
    ask     = Github ask
    local f = Github . local f . unGithub

instance MonadWriter (Sum Int) Github where
    tell = Github . lift . tell
    listen m = do
        r <- ask
        Github . lift . listen . (`runReaderT` r) $ unGithub m
    pass m = do
        (a, fw) <- m
        ((), w) <- listen $ return ()
        writer (a, fw w)

runGithub :: GhAuth -> Github a -> IO (Either SomeException a, Int)
runGithub auth gh =
    fmap (fmap getSum) . runWriterT . runEitherT $ runReaderT (unGithub gh) auth

hoistEitherGH :: Either SomeException a -> Github a
hoistEitherGH = Github . ReaderT . const . EitherT . return

hoistEitherGH' :: Either String a -> Github a
hoistEitherGH' = hoistEitherGH . fmapL (toException . ErrorCall)
