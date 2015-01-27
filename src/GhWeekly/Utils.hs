module GhWeekly.Utils
    ( nubBy
    , countCall
    ) where


import           Control.Arrow
import           Control.Monad.Writer.Strict
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import           Data.Monoid

import           GhWeekly.Types


nubBy :: (Hashable b, Eq b) => (a -> b) -> [a] -> [a]
nubBy f = M.elems . M.fromList . map (f &&& id)

countCall :: Github ()
countCall = tell $ Sum 1
