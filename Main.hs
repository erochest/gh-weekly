{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Control.Lens
import qualified Data.Text                   as T
-- import qualified Data.Text.Lazy              as TL
-- import           Data.Text.Lazy.Builder
-- import           Network.Wreq
import           Options.Applicative         hiding (header)
import qualified Options.Applicative         as O
import           Text.Blaze.Html5            hiding (header, option)
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A


data GhWeekly
        = GhWeekly
        { _ghwUser :: !T.Text
        , _ghwOrgs :: !(Maybe T.Text)
        } deriving (Show)
makeLenses ''GhWeekly

data User
data Repo
data Event
data Commit

data RepoReport
        = RepoReport
        { _rrRepo        :: !Repo
        , _rrIssueEvents :: ![Event]
        , _rrCommits     :: ![Commit]
        }
makeLenses ''RepoReport

data UserReport
        = UserReport
        { _reportUser :: !User
        , _reportRepo :: !RepoReport
        }
makeLenses ''UserReport


renderUser :: UserReport -> Html
renderUser = undefined

renderRepo :: RepoReport -> Html
renderRepo = undefined

renderIssueEvent :: Event -> Html
renderIssueEvent = undefined

renderCommit :: Commit -> Html
renderCommit = undefined


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
           <> O.header "gh-weekly -- report a user's Github activity.")

textReader :: ReadM T.Text
textReader = T.pack <$> str
