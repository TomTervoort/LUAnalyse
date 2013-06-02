{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.State where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)
import Data.Maybe

import qualified Data.PartitionedSet as PS

import LUAnalyse.ControlFlow.Flow

-- Flow generation state.
type Variables       = (PS.PartitionedSet Variable, [PS.PartitionedSet Variable])
type BlockReferences = (BlockReference, BlockReference, BlockReference)
type FlowState       = (
        Map FunctionReference Function,
        Map BlockReference Block,
        [Instruction],
        Variables,
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
initialFlowState = (empty, empty, [], (PS.empty, []), 0, 0, 0, initialBlockRefs)

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
finishFunction :: FlowState -> BlockReference -> BlockReference -> [Variable] -> Variable -> State FlowState FunctionReference
finishFunction oldState entryBlockRef exitBlockRef params retVar = do
    -- Fetch new state.
    newState <- get
    (functions, newBlocks, _, variables, varCounter, blockCounter, funcCounter, blockRefs) <- return newState
    
    -- And old state.
    -- TODO: The original code reinstated the old `blockRefs`. I'm not convinced
    --       that this is desirable.
    (_, oldBlocks, instructions, _, _, _, _, _) <- return oldState
    
    -- Create function.
    function <- return Function {flow = newBlocks, entry = entryBlockRef, exit = exitBlockRef, params = params, returnVar = retVar}
    
    -- Add function.
    newFunctions <- return $ insert funcCounter function functions
    
    -- Merge states.
    put (newFunctions, oldBlocks, instructions, variables, varCounter, blockCounter, funcCounter + 1, blockRefs)
    
    -- Return reference.
    return funcCounter

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState ()
appendInstruction instr = do
    modify $ \ (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) ->
        (functions, blocks, instructions ++ [instr], variables, varCounter, blockCounter, funcCounter, blockRefs)

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

-- Starts a block. Returns its reference.
startBlock :: BlockReference -> State FlowState ()
startBlock blockRef = do
    (functions, blocks, _, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (_, exitBlockRef, breakBlockRef) <- return blockRefs
    
    newBlockRefs <- return (blockRef, exitBlockRef, breakBlockRef)
    put (functions, blocks, [], variables, varCounter, blockCounter, funcCounter, newBlockRefs)

-- Finishes a block. Returns its reference.
finishBlock :: FlowInstruction -> State FlowState ()
finishBlock flowInstr = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (currentBlockRef, exitBlockRef, breakBlockRef) <- return blockRefs
    
    block <- return $ Block instructions flowInstr
    
    newBlockRefs <- return (unavailableBlockRef, exitBlockRef, breakBlockRef)
    put (functions, insert currentBlockRef block blocks, [], variables, varCounter, blockCounter, funcCounter, newBlockRefs)

-- Starts a variable scope.
startScope :: State FlowState ()
startScope = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (globals, scoped) <- return variables
    
    -- Add scope.
    newVariables <- return (globals, PS.empty : scoped)
    
    put (functions, blocks, instructions, newVariables, varCounter, blockCounter, funcCounter, blockRefs)

-- Ends a scope.
endScope :: State FlowState ()
endScope = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (globals, scoped) <- return variables
    
    -- Remove scope.
    newVariables <- return (globals, tail scoped)
    
    put (functions, blocks, instructions, newVariables, varCounter, blockCounter, funcCounter, blockRefs)

-- Gets a variable by name. The variable will be created as a global it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (globals, scoped) <- return variables
    
    case catMaybes . map (flip PS.mrep (Variable name)) $ scoped ++ [globals] of
        (v:_) -> do return v
        []      -> do
                        newVar       <- return (Variable name)
                        newVariables <- return (PS.insert newVar globals, scoped)
                        put (functions, blocks, instructions, newVariables, varCounter, blockCounter, funcCounter, blockRefs)
                        return newVar

-- Creates a local variable.
createVariable :: String -> State FlowState Variable
createVariable name = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (globals, scoped) <- return variables
    
    -- Add variable.
    newVar       <- return $ FreshVariable varCounter
    newVariables <- return (globals, (PS.insert newVar (head scoped)) : tail scoped)
    
    put (functions, blocks, instructions, newVariables, varCounter + 1, blockCounter, funcCounter, blockRefs)
    return newVar

-- Gets a new unique variable name.
getNewVariable :: State FlowState Variable
getNewVariable = do
    (functions, blocks, instructions, variables, varCounter, blockCounter, funcCounter, blockRefs) <- get
    (globals, scoped) <- return variables
    
    -- Add global variable.
    newVar       <- return $ FreshVariable varCounter
    newVariables <- return (PS.insert newVar globals, scoped)
    
    put (functions, blocks, instructions, newVariables, varCounter + 1, blockCounter, funcCounter, blockRefs)
    
    return newVar

