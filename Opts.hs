{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( parseArgs
    , execParser
    , opts'
    , opts
    ) where


import           Data.Text
import           Options.Applicative

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

opts :: ParserInfo GhWeekly
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Produce a report of a user's Github activity for a week."
            <> header "gh-weekly -- report a user's Github activity.")

textReader :: ReadM Text
textReader = pack <$> str
