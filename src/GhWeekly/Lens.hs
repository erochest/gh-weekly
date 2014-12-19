{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Lens
    ( author
    , commit
    , date
    , message
    , url
    ) where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens


author, commit, date, message, url
    :: AsValue t => Traversal' t Value

author  = key "author"
commit  = key "commit"
date    = key "date"
message = key "message"
url     = key "url"
