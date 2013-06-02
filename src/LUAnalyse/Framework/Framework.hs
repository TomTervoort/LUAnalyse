{-# LANGUAGE Haskell2010, FunctionalDependencies, ScopedTypeVariables #-}


module LUAnalyse.Framework.Framework (
                                        module LUAnalyse.ControlFlow.Flow,
                                        Analysis (..),
                                        AnalysisType (..),
                                        AnalysisDirection (..),
                                        performAnalysis,
                                        
                                        
                                        labelInstructions,
                                        nextInstructions,
                                        edges
                                     ) where

import LUAnalyse.Framework.Lattice
import LUAnalyse.ControlFlow.Flow 

import Control.Arrow
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- | Denotes the direction of analysis: either forward or backwards.
data AnalysisDirection = ForwardAnalysis
                       | BackwardAnalysis

-- | Denotes the type of analysis. A 'must' analysis uses, by default, a lattice top as least 
--   informative value and uses the lattice meet to restrict is as the analysis progresses.
--   A 'may' analysis, on the other hand, uses the lattice bottom as least informative value by 
--   default and extends it by joining.
-- 
--   You could say that, in a must analysis information is restricted/specialized while, in a may
--   analysis, information is extended/generalised.
data AnalysisType = MustAnalysis
                  | MayAnalysis

-- | A type class representing an analysis of Lua programs within the monothonic framework. Here 
--   @a@ is a datatype representing the analysis. It may be a simple marker unit type but might
--   also contain parameters or settings for the analysis (note that default definitions and
--   framework functions will never evaluate a, so it may be a zero type). @l@ is a lattice in 
--   which information is stored that is gathered by the analysis (e.g. the available expressions
--   or currently known types of variables).
class Lattice l => Analysis a l | a -> l where
 -- | A monotonic transfer function that updates the lattice based on the effects of a Lua 
 --   instruction.
 transfer :: a -> Instruction -> l -> l

 -- | Provides the type and direction of the analysis.
 analysisKind :: a -> (AnalysisType, AnalysisDirection)

 -- | Returns true if the transfer function is distrubitive, meaning @transfer a i (l1 `join` l2)
 --   == transfer a i l1 `join` transfer a i l2@. Default is @const False@.
 isDistributive :: a -> Bool
 isDistributive _ = False

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
labelInstructions = functions >>> M.elems >>> map flow >>> M.unions >>> M.assocs >>> concatMap (uncurry blockLabels) >>> M.fromList
 where blockLabels :: BlockReference -> Block -> [(InstructionLabel, Instruction)]
       blockLabels ref (Block is _) = [(InstructionLabel ref ind, ins) | (ind, ins) <- zip [0..] is]

-- | Gives the set of labels of instructions that are be executed after the one at a particular 
--   label.
nextInstructions :: Program -> Maybe InstructionLabel -> [InstructionLabel]
nextInstructions p Nothing = 
  concatMap (\func -> nextInstructions p (Just $ InstructionLabel (entry func) (-1))) $ M.elems $ functions p
nextInstructions p (Just (InstructionLabel ref ind))
    = case ref `M.lookup` wholeFlow of
        Nothing -> error "LUAnalyse.Framework#L101"
        Just (Block ins jump)
            | ind < length ins - 1 -> [InstructionLabel ref (ind + 1)]
            | otherwise -> firstIns jump

 where firstIns jump = 
        case jump of
         JumpInstr x         -> firstIns' x
         CondJumpInstr a b _ -> firstIns' a ++ firstIns' b
         ReturnInstr         -> []
       firstIns' r = 
        case r `M.lookup` wholeFlow of
         Nothing    -> error "LUAnalyse.Framework#L113"
         Just (Block [] j) -> firstIns j
         Just (Block _ _)  -> [InstructionLabel r 0]
       wholeFlow = M.unions $ map flow $ M.elems $ functions p

-- | Provides the extremal labels of the program for a given direction.
extremalLabels :: Program -> AnalysisDirection -> [InstructionLabel]
extremalLabels p ForwardAnalysis = nextInstructions p Nothing
extremalLabels p BackwardAnalysis = until (null . next) next $ nextInstructions p Nothing
 where next = concatMap (nextInstructions p . Just)

-- | Provides the edges of the program graph as (from,to) tuples. These are flipped for 
--   BackwardsAnalysis.
edges :: Program -> AnalysisDirection -> [(InstructionLabel, InstructionLabel)]
edges p dir = [mayflip (l, l') 
                | l <- M.keys $ labelInstructions p, l' <- nextInstructions p $ Just l]
 where mayflip (a,b) = case dir of
                        ForwardAnalysis  -> (a,b)
                        BackwardAnalysis -> (b,a)

-- | Turns the result of @edges@ into a graph representation that maps nodes to neighbours. This
--   allows fast lookup of a particular instruction.
graphRep :: [(InstructionLabel, InstructionLabel)] -> Map InstructionLabel [InstructionLabel]
graphRep = foldr (\(n,e) -> M.alter (addEdge e) n) M.empty
 where addEdge e es = Just $ e : fromMaybe [] es

-- | Performs the actual analysis using maximal fixpoint iteration. The result is a mapping of
--   labels to the eventual state of the lattice before and after exiting the label (in the 
--   direction of the analysis).
performAnalysis :: forall a l. Analysis a l => a -> Program -> Map InstructionLabel (l, l)
performAnalysis a p = addExits $ mfp edges' initialState
 where (atype, adir) = analysisKind a
       trans = transfer a
       edges' = edges p adir
       graph = graphRep edges'
       instructions = labelInstructions p
       outgoing l = [(l, l') | l' <- maybe [] id $ l `M.lookup` graph]
       extremals = S.fromList $ extremalLabels p adir
       initialVal l | l `S.member` extremals = extremal a
                    | otherwise              = least a

       -- The intial state of the information about each label.
       initialState :: Map InstructionLabel l
       initialState = M.mapWithKey (\l _ -> initialVal l) instructions

       -- MFP iteration, provides the entry value of each label.
       mfp :: [(InstructionLabel, InstructionLabel)] 
                -> Map InstructionLabel l 
                -> Map InstructionLabel l
       mfp []          m = m
       mfp ((l,l'):ls) m | not (transferred </ ml') = mfp (outgoing l' ++ ls) 
                                                       $ M.insert l' (combine a ml' transferred) m
                         | otherwise = mfp ls m
        where transferred = trans (instructions M.! l) $ m M.! l
              ml' = maybe (error "LUAnalyse.Framework#L167") id  $ l' `M.lookup` m

       -- Performs the transfer function on each label once more to obtain the exit value along 
       -- with the entry value.
       addExits :: Map InstructionLabel l -> Map InstructionLabel (l, l)
       addExits = M.mapWithKey $ \i l -> (l, trans (instructions M.! i) l)
