{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe
import qualified Data.Text.IO               as TIO
import           Data.Time
import           Data.Traversable           hiding (mapM)
import           System.Exit
import           System.IO

import           GhWeekly.Lens
import           GhWeekly.Network
import           GhWeekly.Report
import           GhWeekly.Types

import           Opts


-- TODO: use a set of commits, by hash, so there aren't duplicates after
-- a merge

-- TODO: give a summary of how many commits, how many files, and how many
-- lines

-- TODO: html output

-- TODO: issue activity (opened, closed, and contributed to)

watch :: Show a => a -> IO a
watch x = putStrLn ("WATCH: " ++ show x) >> return x

exitEither :: Either SomeException a -> IO ()
exitEither (Left err) = hPrint stderr err >> exitFailure
exitEither (Right _)  = exitSuccess

main :: IO ()
main = do
    GhWeekly{..} <- parseArgs
    since <-  addUTCTime (fromIntegral $ _ghwDays * 24 * 60 * 60 * (-1))
          <$> getCurrentTime

    putStrLn "Querying github..."
    exitEither =<< runGithub _ghwOauthToken (do
        userRepos <-  getAllUserRepos _ghwUser
        orgRepos  <-  fmap concat
                  .   mapM getOrgRepos
                  .   mapMaybe (preview (login . _String))
                  =<< getUserOrgs _ghwUser

        mapM_ (liftIO . TIO.putStr . uncurry renderCommits)
            =<< ( mapM (sequenceA . (id &&& getRepoCommitsFor' _ghwUser since))
                . mapMaybe (preview (fullName . _String))
                $ userRepos ++ orgRepos)
        )
    where
        getRepoCommitsFor' u s r = getAllCommits r u s
