{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Contains a simple analysis that determines which variables in the program have never been 
--   assigned to after intialization and are therefore constant. This is very useful for functions,
--   because with this knowledge we know when we can treat them as non-higher order procedures.
module LUAnalyse.Analysis.Constants (performConstantsAnalysis) where

import LUAnalyse.Framework.Lattice
import LUAnalyse.Framework.Framework

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- | A mapping from variables to booleans indicating whether they have only been assigned to once 
--   so far. Variables that are not present in the map are considered to have value @NoAssignment@.
data ConstantVars = ConstantVars (Map Variable AssignmentStatus)
                  | NoConstantVars -- Top value, implies all variables have MultipleAssignments.

-- | Holds whether the number of assignments to a variable is 0, 1 or >= 2.
data AssignmentStatus = NoAssignments
                      | OneAssignment
                      | MultipleAssignments
                      deriving (Eq, Ord)
   
-- | Returns the assignment status of a variable. If this is @OneAssignment@, the variable has been
--   constant up to this point.
getAssignmentStatus :: Variable -> ConstantVars -> AssignmentStatus
getAssignmentStatus _ NoConstantVars   = MultipleAssignments
getAssignmentStatus v (ConstantVars m) = M.findWithDefault NoAssignments v m

-- | Updates the ConstantVars lattice with the notion that a variable can be assigned in a location.
updateWithAssignment :: Variable -> ConstantVars -> ConstantVars
updateWithAssignment _ NoConstantVars = NoConstantVars
updateWithAssignment v (ConstantVars m) = ConstantVars $ M.alter addVar v m
 where addVar Nothing              = Just OneAssignment
       addVar (Just NoAssignments) = Just OneAssignment
       addVar _                    = Just MultipleAssignments

-- | @AssignmentStatus@ itself forms a simple lattice.
instance Lattice AssignmentStatus where
 (</) = (<=)
 join = max
 meet = min
 bottom = NoAssignments
 top = MultipleAssignments

instance Lattice ConstantVars where
 aa </ bb = 
  case (aa,bb) of
   (ConstantVars a, ConstantVars b) -> all (\(v, s) -> s </ M.findWithDefault NoAssignments v b) 
                                        $ M.assocs a
   (_             , NoConstantVars) -> True
   (NoConstantVars, ConstantVars _) -> False
 join aa bb = 
  case (aa,bb) of
   (ConstantVars a, ConstantVars b) -> ConstantVars $ M.unionWith join a b
   (_             , _             ) -> NoConstantVars
 meet aa bb =
  case (aa,bb) of
   (ConstantVars a, ConstantVars b) -> ConstantVars $ M.intersectionWith meet a b
   (ca            , NoConstantVars) -> ca
   (NoConstantVars, cb            ) -> cb
 bottom = ConstantVars M.empty
 top = NoConstantVars

-- | Unit datatype representing constants analysis, which checks for each variable, in each program
--   point, whether they have been assigned to 0, 1 or >= 2 times. From this it can be derived 
--   which variables will certainly remain constant up to a certain point.
data ConstantsAnalysis = ConstantsAnalysis

instance Analysis ConstantsAnalysis ConstantVars where
 analysisKind _ = (MayAnalysis, ForwardAnalysis)
 transfer _ ins = updateWithAssignment (var ins)

-- | Performs constants analysis on a program. A mapping is returned from instruction labels to 
--   variables of which it is certain that they are constant before entering the label.
performConstantsAnalysis :: Program -> Map InstructionLabel (Set Variable)
performConstantsAnalysis = M.map (fetch . fst) . performAnalysis ConstantsAnalysis 
 where fetch NoConstantVars   = S.empty
       fetch (ConstantVars m) = M.keysSet m
