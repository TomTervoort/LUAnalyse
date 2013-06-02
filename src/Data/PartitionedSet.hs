module Data.PartitionedSet
    ( PartitionedSet
    , empty
    , insert
    , join
    , setOf
    , partitionOf
    , mrep
    ) where

import Data.Monoid

import qualified Data.Partition as P
import qualified Data.Set as S

data PartitionedSet a
    = PS_ !(S.Set a) !(P.Partition a)

instance (Ord a) => Monoid (PartitionedSet a) where
    mempty = empty
    mappend (PS_ xs xp) (PS_ ys yp)
      = let zs = xs `mappend` ys
            zp = foldr allEqual yp . P.nontrivialSets $ xp

            allEqual :: (Ord a) => S.Set a -> P.Partition a -> P.Partition a
            allEqual s p = case S.elems s of
                []   -> p
                x:xs -> foldr (P.join x) p xs

        in PS_ zs zp

empty :: PartitionedSet a
empty = PS_ S.empty P.discrete

insert :: (Ord a) => a -> PartitionedSet a -> PartitionedSet a
insert x (PS_ s p) = PS_ (x `S.insert` s) p

join :: (Ord a) => a -> a -> PartitionedSet a -> PartitionedSet a
join x y (PS_ s p)
    | x `S.member` s, y `S.member` s = PS_ s (P.join x y p)
    | otherwise = error "joining non-elements"

setOf :: PartitionedSet a -> S.Set a
setOf (PS_ s _) = s

partitionOf :: PartitionedSet a -> P.Partition a
partitionOf (PS_ _ p) = p

mrep :: (Ord a) => PartitionedSet a -> a -> Maybe a
mrep (PS_ s p) x
    | x `S.member` s = Nothing
    | otherwise = Just (P.rep p x)
