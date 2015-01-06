{-# LANGUAGE OverloadedStrings #-}


module GhWeekly.Lens
    ( name
    , author
    , commit
    , date
    , fullName
    , login
    , message
    , url
    ) where


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens


name, author, commit, date, fullName, login, message, url
    :: AsValue t => Traversal' t Value

name     = key "name"
author   = key "author"
commit   = key "commit"
date     = key "date"
fullName = key "full_name"
login    = key "login"
message  = key "message"
url      = key "url"
