{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Report
    ( render
    , renderUser
    , renderRepo
    , renderObject
    , renderCommits
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable          hiding (foldr)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import qualified Data.Text.Format       as F
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Vector            as V
import           Text.Blaze.Html5
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A

import           GhWeekly.Lens
import           GhWeekly.Types


render :: UserReport -> T.Text
render = undefined

renderUser :: UserReport -> Html
renderUser = undefined

renderRepo :: RepoReport -> Html
renderRepo = undefined

renderObject :: Object -> Html
renderObject = undefined

renderCommits :: T.Text -> [Value] -> T.Text
renderCommits _ [] = T.empty
renderCommits repo commits = TL.toStrict . toLazyText $
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

        header = F.build "# {}\n\n" $ F.Only repo
        footer = F.build "{} commits.\n{} files.\n{} lines.\n\n"
                         (padS $ length commits, padS files, padS $ round lines)
