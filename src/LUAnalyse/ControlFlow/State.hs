{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.State where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)

import LUAnalyse.ControlFlow.Flow

-- Flow generation state.
type BlockReferences = (BlockReference, BlockReference, BlockReference)
type FlowState       = (
        Map FunctionReference Function,
        Map BlockReference Block,
        [Instruction],
        Map String Variable,
        Int,
        BlockReference,
        FunctionReference,
        BlockReferences
    )

-- blockRefs = (currentBlockRef, exitBlockRef, breakBlockRef)
-- flowState = (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs)

-- Unavailable block reference.
unavailableBlockRef :: BlockReference
unavailableBlockRef = -1

-- Starting block references.
initialBlockRefs :: BlockReferences
initialBlockRefs = (unavailableBlockRef, unavailableBlockRef, unavailableBlockRef)

-- Starting state.
initialFlowState :: FlowState
initialFlowState = (empty, empty, [], empty, 0, 0, 0, initialBlockRefs)

-- Starts a function.
startFunction :: State FlowState FlowState
startFunction = do
    -- Fetch old state.
    oldState <- get
    (functions, _, _, variables, varCounter, blockCounter, funcCounter, _) <- return oldState
    
    -- Set blocks, instructions and block references to initial state.
    put (functions, empty, [], variables, varCounter, blockCounter, funcCounter, initialBlockRefs)
    
    return oldState

-- Finishes a function.
finishFunction :: FlowState -> BlockReference -> BlockReference -> [Variable] -> State FlowState FunctionReference
finishFunction oldState entryBlockRef exitBlockRef params = do
    -- Fetch new state.
    newState <- get
    (functions, newBlocks, _, variables, varCounter, blockCounter, funcCounter, blockRefs) <- return newState
    
    -- And old state.
    (_, oldBlocks, instructions, _, _, _, _, blockRefs) <- return oldState
    
    -- Create function.
    function <- return Function {flow = newBlocks, entry = entryBlockRef, exit = exitBlockRef, params = params}
    
    -- Add function.
    newFunctions <- return $ insert funcCounter function functions
    
    -- Merge states.
    put (newFunctions, oldBlocks, instructions, variables, varCounter, blockCounter, funcCounter + 1, blockRefs)
    
    -- Return reference.
    return funcCounter

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState Variable
appendInstruction instr = do
    modify $ \ (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) ->
        (functions, blocks, instructions ++ [instr], variables, varCounter, blockCounter, funcCounter, blockRefs)
    return $ var instr

-- Gets a new block reference.
getBlockReference :: State FlowState BlockReference
getBlockReference = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    put (functions, blocks, instructions, variables, varCounter, blockCounter + 1, funcCounter, blockRefs)
    return blockCounter

-- Gets block reference.
getBlockReferences :: State FlowState BlockReferences
getBlockReferences = do
    (_, _, _, _, _, _, _, blockRefs) <- get
    return blockRefs

-- Sets block reference.
setBlockReferences :: BlockReferences -> State FlowState ()
setBlockReferences blockRefs = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, _) <- get
    put (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs)
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
    (functions, blocks, _, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (_, exitBlockRef, breakBlockRef) <- return blockRefs
    
    newBlockRefs <- return (blockRef, exitBlockRef, breakBlockRef)
    put (functions, blocks, [], variables, varCounter, blockCounter, funcCounter, newBlockRefs)
    
    return blockRef

-- Finishes a block. Returns its reference.
finishBlock :: FlowInstruction -> State FlowState BlockReference
finishBlock flowInstr = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (currentBlockRef, exitBlockRef, breakBlockRef) <- return blockRefs
    
    block <- return $ Block instructions flowInstr
    
    newBlockRefs <- return (unavailableBlockRef, exitBlockRef, breakBlockRef)
    put (functions, insert currentBlockRef block blocks, [], variables, varCounter, blockCounter, funcCounter, newBlockRefs)
    
    return currentBlockRef

-- Creates a variable alias.
createVariableAlias :: String -> Variable -> State FlowState Variable
createVariableAlias name var = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    put (functions, blocks, instructions, insert name var variables, varCounter, blockCounter, funcCounter, blockRefs)
    return var

-- Gets a variable by name. The variable will be created if it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    case lookup name variables of
        Just var -> do return var
        Nothing -> do
            put (functions, blocks, instructions, newVariables, varCounter, blockCounter, funcCounter, blockRefs)
            return newVar
                where
                    newVariables = insert name newVar variables
                    newVar       = Variable name

-- Looks up a variable by name.
lookupVariable :: String -> State FlowState (Maybe Variable)
lookupVariable name = do
    (_, _, _, variables, _, _, _, _) <- get
    return $ lookup name variables

-- Gets a new unique variable name.
getNewVariable :: State FlowState Variable
getNewVariable = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    put (functions, blocks, instructions, variables, varCounter + 1, blockCounter, funcCounter, blockRefs)
    getVariable $ "%" ++ show varCounter
