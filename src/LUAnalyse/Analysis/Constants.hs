{-# LANGUAGE Haskell2010, FlexibleInstances #-}

-- | Contains a simple analysis that determines which variables in the program have never been 
--   assigned to after intialization and are therefore constant. This is very useful for functions,
--   because with this knowledge we know when we can treat them as non-higher order procedures.
module LUAnalyse.Analysis.Constants where

import LUAnalyse.Framework.Lattice
import LUAnalyse.Framework.Framework

import Data.Map (Map)
import qualified Data.Map as M

-- | A mapping from variables to booleans indicating whether they have only been assigned to once 
--   so far. Variables that are not present in the map are considered to have value @NoAssignment@.
data ConstantVars = ConstantVars (Map Variable AssignmentStatus)
                  | NoConstantVars -- Top value, implies all variables have MultipleAssignments.

-- | Holds whether the number of assignments to a variable is 0, 1 or >= 2.
data AssignmentStatus = NoAssignments
                      | OneAssignment
                      | MultipleAssignments
                      deriving (Eq, Ord)
   
-- | Updates the ConstantVars lattice with the notion that a variable can be assigned in a location.
updateWithAssignment :: Variable -> ConstantVars -> ConstantVars
updateWithAssignment _ NoConstantVars = NoConstantVars
updateWithAssignment v (ConstantVars m) = ConstantVars $ M.alter addVar v m
 where addVar Nothing              = Just OneAssignment
       addVar (Just NoAssignments) = Just OneAssignment
       addVar _                    = Just MultipleAssignments

-- | @AssignmentStatus@ itself forms a lattice.
instance Lattice AssignmentStatus where
 (</) = (<=)
 join = max
 meet = min
 bottom = NoAssignments
 top = MultipleAssignments

instance Lattice ConstantVars where
 a </ b = 
  case (a,b) of
   (ConstantVars a, ConstantVars b) -> all (\(v, s) -> s </ M.findWithDefault NoAssignments v b) 
                                        $ M.assocs a
   (_             , NoConstantVars) -> True
   (NoConstantVars, ConstantVars _) -> False
 join a b = 
  case (a,b) of
   (ConstantVars a, ConstantVars b) -> ConstantVars $ M.unionWith join a b
   (_             , _             ) -> NoConstantVars
 meet a b =
  case (a,b) of
   (ConstantVars a, ConstantVars b) -> ConstantVars $ M.intersectionWith meet a b
   (ca            , NoConstantVars) -> ca
   (NoConstantVars, cb            ) -> cb
 bottom = ConstantVars M.empty
 top = NoConstantVars


 