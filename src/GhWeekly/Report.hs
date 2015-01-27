{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Report
    ( renderObject
    , renderCommits
    , renderIssues
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import qualified Data.Text.Format       as F
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Vector            as V
import           Prelude                hiding (foldr)
import           Text.Blaze.Html5
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A

import           GhWeekly.Lens
import           GhWeekly.Types


renderObject :: T.Text -> [Value] -> [Value] -> T.Text
renderObject repoName commits issues = TL.toStrict . toLazyText $
    header <> renderCommits repoName commits <> renderIssues repoName issues
    where
        header = F.build "# {}\n\n" $ F.Only repoName

renderCommits :: T.Text -> [Value] -> Builder
renderCommits _ [] = mempty
renderCommits repo commits =
    header <> foldr renderCommit "\n" commits <> footer
    where
        renderCommit v =
            mappend (foldMap rc $ (,) <$> v ^? commit . author . date . _String
                                      <*> v ^? commit . message . _String)
        rc (d, m) = F.build "[{}] {}\n" (d, firstLine m)
        firstLine = fromMaybe T.empty . listToMaybe . T.lines

        sumOn prism = maybe mempty Sum . preview prism
        fileCount   = key "files" . _Array . to V.length
        lineCount   = key "stats" . key "total" . _Number
        (Sum files, Sum lines) =
            foldMap (sumOn fileCount &&& sumOn lineCount) commits
        padS = F.left 5 ' ' . F.Shown

        header = "## Commits\n\n"
        footer = F.build "{} commits.\n{} files.\n{} lines.\n\n"
                         (padS $ length commits, padS files, padS $ round lines)

renderIssues :: T.Text -> [Value] -> Builder
renderIssues _ [] = mempty
renderIssues repo issues =
    header <> foldr renderIssue "\n" issues' <> footer
    where
        issues' = V.concat $ issues ^.. traverse . key "items" . _Array

        renderIssue :: Value -> Builder -> Builder
        renderIssue  i = mappend (renderIssue' i)
        renderIssue' i =
            F.build "#{}: [{}] {}\n"
                ( F.Shown . fromMaybe ((-1) :: Int) $ i ^? key "number" . _Integral
                , state $ i ^? key "state" . _String
                , fromMaybe "<Untitled>" $ i ^? key "title" . _String
                )

        state :: Maybe T.Text -> Builder
        state (Just "open")   = " "
        state (Just "closed") = "*"
        state Nothing         = "?"

        header = "## Issues\n\n"
        footer = F.build "{} issues.\n\n" . F.Only $ V.length issues'
