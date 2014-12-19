{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Report
    ( render
    , renderUser
    , renderRepo
    , renderObject
    , renderCommits
    ) where


import Control.Applicative
import Data.Foldable hiding (foldr)
import Data.Monoid
import Control.Lens
import Data.Aeson.Lens
import           Data.Aeson
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import           Text.Blaze.Html5
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A

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
-- renderCommits _ [] = T.empty
renderCommits repo commits = TL.toStrict . toLazyText $
    "# " <> fromText repo <> "\t" <> fromString (show $ length commits) <> "\n"
    -- "# " <> fromText repo <> "\n\n" <> foldr renderCommit "\n\n" commits
    where
        renderCommit v =
            mappend (foldMap rc $ (,) <$> v ^? key "commit"
                                            .  key "author"
                                            .  key "date"
                                            .  _String
                                      <*> v ^? key "commit"
                                            .  key "message"
                                            .  _String)
        rc (date, msg) = mconcat ["[", fromText date, "] ", fromText msg]
