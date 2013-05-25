{-# LANGUAGE Haskell2010, FunctionalDependencies #-}


module LUAnalyse.Framework.Framework where

import LUAnalyse.Framework.Lattice
import LUAnalyse.ControlFlow.Flow 

import Data.Map (Map)
import qualified Data.Map as M

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

 -- | The value of the lattice before entering the extremal labels. The default is top for must 
 --   analyses, and bottom for may analyses.
 extremal :: a -> l
 extremal a = case fst $ analysisKind a of
               MustAnalysis -> top
               MayAnalysis  -> bottom

 -- | Combines the latice values of a previous state when they are flown inside a transfer function.
 --   The default operation for must analyses is @intersection@, and @union@ for may analyses.
 combinator :: a -> [l] -> l
 combinator a = case fst $ analysisKind a of
                 MustAnalysis -> intersection
                 MayAnalysis  -> union

performAnalysis :: Analysis a l => Program -> a -> Map InstructionLabel l
performAnalysis = undefined
