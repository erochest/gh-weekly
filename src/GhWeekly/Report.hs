{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Report
    ( render
    , renderUser
    , renderRepo
    , renderObject
    ) where


import           Data.Aeson
import qualified Data.Text        as T
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
