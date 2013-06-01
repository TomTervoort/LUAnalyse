{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Formatter(formatControlFlow) where

import LUAnalyse.ControlFlow.Flow

import Data.Map hiding (map, member)

-- Formats flow.
formatControlFlow :: Program -> String
formatControlFlow (Program {functions = functions}) = foldrWithKey f "" functions
    where
        f reference function acc = (formatFunction reference function) ++ "\n" ++ acc

-- Formats flow.
formatFunction :: FunctionReference -> Function -> String
formatFunction funcReference (Function {flow = flow, params = params, returnVar = returnVar}) = 
    "function " ++ (show funcReference) ++
        ":\n\targs = " ++ (show params) ++ "\n\tretvar = " ++ (show returnVar) ++ "\n\n" ++ (foldrWithKey f "" flow)
        where
            f blockReference block acc = (formatBlock blockReference block) ++ acc

-- Formats block.
formatBlock :: BlockReference -> Block -> String
formatBlock reference (Block instructions flowInstr) =
    "\tblock " ++ (show reference) ++ ":\n" ++ (concatMap formatInstruction instructions) ++ "\t\t" ++ show flowInstr ++ "\n"

-- Formats an instruction.
formatInstruction :: Instruction -> String
formatInstruction instr = "\t\t" ++ show instr ++ "\n"
