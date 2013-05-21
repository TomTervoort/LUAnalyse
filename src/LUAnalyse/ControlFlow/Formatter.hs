{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Formatter(formatControlFlow) where

import LUAnalyse.ControlFlow.Flow

import Data.Map hiding (map, member)

-- Formats flow.
formatControlFlow :: Flow -> String
formatControlFlow flow = foldrWithKey f "" flow
    where
        f reference block acc = (formatBlock reference block) ++ acc

-- Formats block.
formatBlock :: BlockReference -> Block -> String
formatBlock reference (Block instructions flowInstr) =
    "block " ++ (show reference) ++ ":\n" ++ (concatMap formatInstruction instructions) ++ "\t" ++ show flowInstr ++ "\n"

-- Formats an instruction.
formatInstruction :: Instruction -> String
formatInstruction instr = "\t" ++ show instr ++ "\n"
