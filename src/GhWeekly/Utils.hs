
module GhWeekly.Utils
    ( decamel
    ) where


import           Data.Char
import qualified Data.List as L


decamel :: String -> String
decamel []     = []
decamel "ID"   = "id"
decamel (x:xs) = toLower x : L.concatMap decamel' xs
    where
        decamel' y | isUpper y = ['_', toLower y]
                   | otherwise = [y]
