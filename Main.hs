{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.Text           as T
import           Options.Applicative


data GhWeekly
        = GhWeekly
        { ghwUser :: !T.Text
        , ghwOrgs :: !(Maybe T.Text)
        } deriving (Show)


main :: IO ()
main = print =<< execParser opt


opt' :: Parser GhWeekly
opt' =   GhWeekly
     <$> option textReader
                (  short 'u' <> long "user" <> metavar "USERNAME"
                <> help "The user to report on.")
     <*> (optional . option textReader
                   $  short 'o' <> long "org" <> metavar "ORGNAME"
                   <> help "An optional organization to look for\
                           \ commits in also.")

opt :: ParserInfo GhWeekly
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Produce a report of a user's Github activity for a week."
           <> header "gh-weekly -- report a user's Github activity.")

textReader :: ReadM T.Text
textReader = T.pack <$> str
