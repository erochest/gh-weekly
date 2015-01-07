module GhWeekly.Utils
    ( nubBy
    ) where


import           Control.Arrow
import           Data.Hashable
import qualified Data.HashMap.Strict as M


nubBy :: (Hashable b, Eq b) => (a -> b) -> [a] -> [a]
nubBy f = M.elems . M.fromList . map (f &&& id)
