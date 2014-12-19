{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson.Lens
import           Data.Maybe
import qualified Data.Text.IO        as TIO
import           Data.Time
import           Data.Traversable    hiding (mapM)
import           System.Exit
import           System.IO

import           GhWeekly.Network
import           GhWeekly.Report
import           GhWeekly.Types

import           Opts


data Hole

watch :: Show a => a -> IO a
watch x = putStrLn ("WATCH: " ++ show x) >> return x

exitEither :: Either SomeException a -> IO ()
exitEither (Left err) = hPrint stderr err >> exitFailure
exitEither (Right _)  = exitSuccess

main :: IO ()
main = do
    GhWeekly{..} <- parseArgs

    since <-  addUTCTime (fromIntegral $ _ghwDays * 24 * 60 * 60)
          <$> getCurrentTime

    exitEither =<< runGithub _ghwOauthToken (do
        userRepos <-  getAllUserRepos _ghwUser
        orgRepos  <-  fmap concat
                  .   mapM getOrgRepos
                  =<< mapMaybe (preview (key "login" . _String))
                  <$> getUserOrgs _ghwUser

        mapM_ (liftIO . TIO.putStrLn . uncurry renderCommits)
            =<< ( mapM (sequenceA . (id &&& getRepoCommitsFor' _ghwUser since))
                . mapMaybe (preview (key "full_name" . _String))
                $ userRepos ++ orgRepos)
        )
    where
        getRepoCommitsFor' u s r = getRepoCommitsFor r u s
