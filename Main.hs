{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson.Lens
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Environment
import           System.Exit
import           System.IO

import           GhWeekly.Network
-- import           GhWeekly.Report
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
    oauthToken <- T.pack <$> (getEnv "GITHUB_TOKEN" :: IO String)

    exitEither =<< (runGithub oauthToken $ do
        orgRepos  <-  fmap concat
                  .   mapM getOrgRepos
                  =<< mapMaybe (preview (key "login" . _String))
                  <$> getUserOrgs _ghwUser
        userRepos <-  getAllUserRepos _ghwUser

        mapM_ (liftIO . TIO.putStrLn)
            .   mapMaybe (preview (key "full_name" . _String))
            $   userRepos ++ orgRepos
        )
