{-# LANGUAGE Haskell2010, FunctionalDependencies, ScopedTypeVariables #-}


module LUAnalyse.Framework.Framework where

import LUAnalyse.Framework.Lattice
import LUAnalyse.ControlFlow.Flow 

import Control.Arrow
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data AnalysisDirection = ForwardAnalysis
                       | BackwardAnalysis

data AnalysisType = MustAnalysis
                  | MayAnalysis

class Lattice l => Analysis a l | a -> l where
 -- | A monotonic transfer function that updates the lattice based on the effects of a Lua 
 --   instruction.
 transfer :: a -> Instruction -> l -> l

 -- | Provides the type and direction of the analysis.
 analysisKind :: a -> (AnalysisType, AnalysisDirection)

 -- | Returns true if the transfer function is distrubitive, meaning @transfer a i (l1 `join` l2)
 --   == transfer a i l1 `join` transfer a i l2@.
 isDistributive :: a -> Bool

 -- | The least informative value of the lattice. The default is top for must analyses, and bottom 
 --   for may analyses.
 least :: a -> l
 least a = case fst $ analysisKind a of
            MustAnalysis -> top
            MayAnalysis  -> bottom

 -- | The value of the lattice before entering the extremal labels. By default, this is equal to
 --   @least@.
 extremal :: a -> l
 extremal = least


 -- | Combines two latice values. The default operation for must analyses is @meet@, and @join@ for
 --   may analyses. It must hold that @combine a x (least a) = x@.
 combine :: a -> l -> l -> l
 combine a = case fst $ analysisKind a of
                 MustAnalysis -> meet
                 MayAnalysis  -> join

 -- | Combines multiple latice values of a previous state when they are flown inside a transfer 
 --   function. The default operation for must analyses is @intersection@, and @union@ for may 
 --   analyses. It should hold that @combinator a == foldr (combine a) (extremal a)@.
 combinator :: a -> [l] -> l
 combinator a = case fst $ analysisKind a of
                 MustAnalysis -> intersection
                 MayAnalysis  -> union

-- | Fetch all instructions from a program as a mapping from labels.
labelInstructions :: Program -> Map InstructionLabel Instruction
labelInstructions = flow >>> M.assocs >>> concatMap (uncurry blockLabels) >>> M.fromList
 where blockLabels :: BlockReference -> Block -> [(InstructionLabel, Instruction)]
       blockLabels ref (Block is _) = [(InstructionLabel ref ind, ins) | (ind, ins) <- zip [0..] is]

extremalLabels :: Program -> AnalysisDirection -> Set InstructionLabel
-- TODO: handle case where first instruction block is empty
extremalLabels p ForwardAnalysis  = S.singleton $ InstructionLabel (entry p) 0
extremalLabels p BackwardAnalysis = fromMaybe S.empty $ findExits $ entry p
 where findExits ref = 
        case M.lookup ref $ flow p of
         Just (Block [] f) -> findExits' f
         Just (Block is f) -> let finalLabel = InstructionLabel ref (length is - 1)
                               in Just $ fromMaybe (S.singleton finalLabel) $ findExits' f
         Nothing           -> error "Invalid BlockReference."
       findExits' f = case f of
                       JumpInstr b         -> findExits b
                       CondJumpInstr a b _ -> do ea <- findExits a
                                                 eb <- findExits b
                                                 return (ea `S.union` eb)
                       ReturnInstr _       -> Nothing
                       ExitInstr           -> Nothing


nextInstructions :: Program -> InstructionLabel -> [InstructionLabel]
nextInstructions p (InstructionLabel ref ind) =
 let Block ins jump = flow p M.! ref
  in if ind < length ins - 1
      then [InstructionLabel ref (ind + 1)]
      else firstIns jump
 where firstIns jump = 
        case jump of
         JumpInstr x         -> firstIns' x
         CondJumpInstr a b _ -> firstIns' a ++ firstIns' b
         ReturnInstr _       -> []
         ExitInstr           -> []
       firstIns' r = 
        case flow p M.! r of
         Block [] j -> firstIns j
         Block _ _  -> [InstructionLabel r 0]

edges :: Program -> [(InstructionLabel, InstructionLabel)]
edges p = [(l, l') | l <- M.keys $ labelInstructions p, l' <- nextInstructions p l]


performAnalysis :: forall a l. Analysis a l => Program -> a -> Map InstructionLabel (l, l)
performAnalysis p a = addExits $ mfp (edges p) initialState
 where (atype, adir) = analysisKind a
       trans = transfer a
       extremals = extremalLabels p adir
       instructions = labelInstructions p
       outgoing l = [(l, l') | l' <- nextInstructions p l]
       initialVal l | l `S.member` extremals = extremal a
                    | otherwise = least a

       initialState :: Map InstructionLabel l
       initialState = M.mapWithKey (\l _ -> initialVal l) instructions

       mfp :: [(InstructionLabel, InstructionLabel)] 
                -> Map InstructionLabel l 
                -> Map InstructionLabel l
       mfp []          m = m
       mfp ((l,l'):ls) m | not (transferred </ ml') = mfp (outgoing l' ++ ls) 
                                                       $ M.insert l' (combine a ml' transferred) m
                         | otherwise = mfp ls m
        where transferred = trans (instructions M.! l) $ m M.! l
              ml' = m M.! l'

       addExits :: Map InstructionLabel l -> Map InstructionLabel (l, l)
       addExits = M.mapWithKey $ \i l -> (l, trans (instructions M.! i) l)