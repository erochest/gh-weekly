{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( parseArgs
    , execParser
    , opts'
    , opts
    ) where


import           Control.Monad       (join)
import           Data.Text
import           Data.Time
import           Options.Applicative
import           System.Locale

import           GhWeekly.Types


parseArgs :: IO GhWeekly
parseArgs = execParser opts

opts' :: Parser GhWeekly
opts' =   GhWeekly
      <$> option textReader
                 (  short 'u' <> long "user" <> metavar "USERNAME"
                 <> help "The user to report on.")
      <*> (optional . option textReader
                    $  short 'o' <> long "org" <> metavar "ORGNAME"
                    <> help "An optional organization to look for\
                            \ commits in also.")
      <*> option auto
                 (  short 'd' <> long "days" <> metavar "DAYS" <> value 7
                 <> help "The number of days to report on. Default = 7.")
      <*> option textReader
                 (  short 't' <> long "token" <> metavar "GITHUB_TOKEN"
                 <> help "The Oauth token to authenticate with Github.")
      <*> (fmap join . optional . option timeReader
                     $  short 's' <> long "since" <> metavar "YYYY-MM-DD"
                     <> help "The date to query for commits since.\
                             \ If given, this overrides --days.\
                             \ The default is one week ago.")
      <*> switch (  short 'v' <> long "verbose" <> help "Enable verbose output.")

opts :: ParserInfo GhWeekly
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Produce a report of a user's Github activity for a week."
            <> header "gh-weekly -- report a user's Github activity.")

textReader :: ReadM Text
textReader = pack <$> str

timeReader :: ReadM (Maybe UTCTime)
timeReader = parseTime defaultTimeLocale "%F" <$> str
