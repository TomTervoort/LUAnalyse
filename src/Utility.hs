module Utility
    ( outerUnionWith
    ) where

import Data.List (foldl')

import qualified Data.Map as M
import qualified Data.Set as S

type OuterUnionSide a b = (Maybe a, Maybe b)

outerUnionWith :: (Ord k) => (Maybe a -> Maybe b -> r) -> M.Map k a -> M.Map k b -> M.Map k r
outerUnionWith combine lhs rhs
  = let leftOnly    ll = (Just ll, Nothing)
        rightOnly   rr = (Nothing, Just rr)
        merger _ ll rr = Just (Just ll, Just rr)
        doMerge = M.mergeWithKey merger (M.map leftOnly) (M.map rightOnly)
    in M.map (uncurry combine) $ doMerge lhs rhs
