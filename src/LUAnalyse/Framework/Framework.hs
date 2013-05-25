{-# LANGUAGE Haskell2010 #-}


module LUAnalyse.Framework.Framework where

import LUAnalyse.Framework.Lattice
import LUAnalyse.ControlFlow.Flow 

data AnalysisDirection = ForwardsAnalysis
                       | BackwardsAnalysis

class Lattice l => Analysis a l | a -> l where
 -- | A monotonic transfer function that updates the lattice based on the effects of a Lua 
 --   instruction.
 transfer :: a -> Instruction -> l -> l

 -- | Whether the analysis in question is a forwards or a backwards analysis.
 direction :: a -> AnalysisDirection

 -- | Returns true if the transfer function is distrubitive, meaning @transfer a i (l1 `join` l2)
 --   == transfer a i l1 `join` transfer a i l2@.
 isDistributive :: a -> Bool


performAnalysis :: Analysis a l => Program -> a -> Map InstructionLabel l
performAnalysis = undefined
