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
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text.IO               as TIO
import           Data.Time
import           Data.Traversable           hiding (mapM)
import           System.Exit
import           System.IO

import           GhWeekly.Lens
import           GhWeekly.Network
import           GhWeekly.Report
import           GhWeekly.Types
import           GhWeekly.Utils

import           Opts


-- TODO: issue activity (opened, closed, and contributed to)

-- TODO: integrate issues with repository reports

-- TODO: use monad-par and a pool to download simultaneously

watch :: Show a => a -> IO a
watch x = putStrLn ("WATCH: " ++ show x) >> return x

exitEither :: Either SomeException a -> (a -> IO ()) -> IO ()
exitEither (Left err) _ = hPrint stderr err >> exitFailure
exitEither (Right x)  f = f x >> exitSuccess

main :: IO ()
main = do
    GhWeekly{..} <- parseArgs
    week <-  addUTCTime (fromIntegral $ _ghwDays * 24 * 60 * 60 * (-1))
         <$> getCurrentTime
    let since = fromMaybe week _ghwSince

    runGithub _ghwOauthToken $
        liftIO . BS.putStrLn . encode =<< getIssuesInvolving _ghwUser since
    return ()

    -- putStrLn "Querying github..."
    {-
     - result <- runGithub _ghwOauthToken $ do
     -     userRepos <-  getAllUserRepos _ghwUser
     -     orgRepos  <-  fmap concat
     -               .   mapM getOrgRepos
     -               .   mapMaybe (preview (login . _String))
     -               =<< getUserOrgs _ghwUser
     -     mapM (sequenceA . (id &&& getRepoCommitsFor' _ghwUser since))
     -         . mapMaybe (preview (fullName . _String))
     -         $ userRepos ++ orgRepos
     - exitEither result $
     -     mapM_ (liftIO . TIO.putStr . uncurry renderCommits)
     -}
    where
        getRepoCommitsFor' u s r =
                mapM (getCommit r)
            .   mapMaybe (preview (sha . _String))
            =<< L.sortBy (comparing (preview (commit . author . date . _String)))
            .   nubBy (preview (sha . _String))
            <$> getAllCommits r u s
