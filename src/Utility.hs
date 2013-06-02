module Utility
    ( outerUnionWith
    ) where
    
import Data.List (foldl')

import qualified Data.Map as M
import qualified Data.Set as S

outerUnionWith :: (Ord k) => r -> (Maybe v -> Maybe v -> r -> r) -> M.Map k v -> M.Map k v -> r
outerUnionWith zero plus lhs rhs
    = foldl' (\acc (ll, rr) -> plus ll rr acc) zero
    . map (\k -> (k `M.lookup` lhs, k `M.lookup` rhs))
    . S.toList
    $ M.keysSet lhs `S.union` M.keysSet rhs
