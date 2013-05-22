{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.State where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)

import LUAnalyse.ControlFlow.Flow

-- Flow generation state.
type BlockReferences = (BlockReference, BlockReference, BlockReference)
type FlowState       = (Map BlockReference Block, [Instruction], Map String Variable, Int, BlockReference, BlockReferences)

-- blockRefs = (currentBlockRef, exitBlockRef, breakBlockRef)
-- flowState = (blocks, instructions, variables, varCounter, blockCounter, blockRefs)

-- Unavailable block reference.
unavailableBlockRef :: BlockReference
unavailableBlockRef = -1

-- Starting block references.
initialBlockRefs :: BlockReferences
initialBlockRefs = (unavailableBlockRef, unavailableBlockRef, unavailableBlockRef)

-- Starting state.
initialFlowState :: FlowState
initialFlowState = (empty, [], empty, 0, 0, initialBlockRefs)

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState Variable
appendInstruction instr = do
    modify $ \ (blocks, instructions, variables, varCounter, blockCounter, blockRefs) ->
        (blocks, instructions ++ [instr], variables, varCounter, blockCounter, blockRefs)
    return $ var instr

-- Gets a new block reference.
getBlockReference :: State FlowState BlockReference
getBlockReference = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRefs) <- get
    put (blocks, instructions, variables, varCounter, blockCounter + 1, blockRefs)
    return blockCounter

-- Gets block reference.
getBlockReferences :: State FlowState BlockReferences
getBlockReferences = do
    (_, _, _, _, _, blockRefs) <- get
    return blockRefs

-- Sets block reference.
setBlockReferences :: BlockReferences -> State FlowState ()
setBlockReferences blockRefs = do
    (blocks, instructions, variables, varCounter, blockCounter, _) <- get
    put (blocks, instructions, variables, varCounter, blockCounter, blockRefs)
    return ()

-- Gets current block.
getCurrentBlockReference :: State FlowState BlockReference
getCurrentBlockReference = do
    (currentBlockRef, _, _) <- getBlockReferences
    return currentBlockRef

-- Gets exit block.
getExitBlockReference :: State FlowState BlockReference
getExitBlockReference = do
    (_, exitBlockRef, _) <- getBlockReferences
    return exitBlockRef

-- Sets continue block.
setExitBlockReference :: BlockReference -> State FlowState ()
setExitBlockReference exitBlockRef = do
    (currentBlockRef, _, breakBlockRef) <- getBlockReferences
    setBlockReferences (currentBlockRef, exitBlockRef, breakBlockRef)
    return ()

-- Gets break block.
getBreakBlockReference :: State FlowState BlockReference
getBreakBlockReference = do
    (_, _, breakBlockRef) <- getBlockReferences
    return breakBlockRef

-- Sets break block.
setBreakBlockReference :: BlockReference -> State FlowState ()
setBreakBlockReference breakBlockRef = do
    (currentBlockRef, exitBlockRef, _) <- getBlockReferences
    setBlockReferences (currentBlockRef, exitBlockRef, breakBlockRef)
    return ()

-- Starts a block. Returns its reference.
startBlock :: BlockReference -> State FlowState BlockReference
startBlock blockRef = do
    (blocks, _, variables, varCounter, blockCounter, blockRefs) <- get
    (_, exitBlockRef, breakBlockRef) <- return blockRefs
    
    newBlockRefs <- return (blockRef, exitBlockRef, breakBlockRef)
    put (blocks, [], variables, varCounter, blockCounter, newBlockRefs)
    
    return blockRef

-- Finishes a block. Returns its reference.
finishBlock :: FlowInstruction -> State FlowState BlockReference
finishBlock flowInstr = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRefs) <- get
    (currentBlockRef, exitBlockRef, breakBlockRef) <- return blockRefs
    
    block <- return $ Block instructions flowInstr
    
    newBlockRefs <- return (unavailableBlockRef, exitBlockRef, breakBlockRef)
    put (insert currentBlockRef block blocks, [], variables, varCounter, blockCounter, newBlockRefs)
    
    return currentBlockRef

-- Creates a variable alias.
createVariableAlias :: String -> Variable -> State FlowState Variable
createVariableAlias name var = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRefs) <- get
    put (blocks, instructions, insert name var variables, varCounter, blockCounter, blockRefs)
    return var

-- Gets a variable by name. The variable will be created if it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRefs) <- get
    case lookup name variables of
        Just var -> do return var
        Nothing -> do
            put (blocks, instructions, newVariables, varCounter, blockCounter, blockRefs)
            return newVar
                where
                    newVariables = insert name newVar variables
                    newVar       = Variable name

-- Looks up a variable by name.
lookupVariable :: String -> State FlowState (Maybe Variable)
lookupVariable name = do
    (_, _, variables, _, _, _) <- get
    return $ lookup name variables

-- Gets a new unique variable name.
getNewVariable :: State FlowState Variable
getNewVariable = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRefs) <- get
    put (blocks, instructions, variables, varCounter + 1, blockCounter, blockRefs)
    getVariable $ "%" ++ show varCounter
