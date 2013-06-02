{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module LUAnalyse.ControlFlow.State where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)
import Data.Maybe
import Data.Lens.Common
import Data.Lens.Template

import LUAnalyse.ControlFlow.Flow

-- Flow generation state.
type Variables       = (Map String Variable, [Map String Variable])
type BlockReferences = (BlockReference, BlockReference, BlockReference)

data FlowState = FlowState
    { _stFunctions  :: Map FunctionReference Function
    , _stBlocks     :: Map BlockReference Block
    , _stInstructions :: [Instruction]
    , _stVariables  :: Variables
    , _stVariableCounter :: Int
    , _stBlockCounter :: BlockReference
    , _stFunctionCounter :: FunctionReference
    , _stBlockReferences :: BlockReferences
    }

$( makeLens ''FlowState )

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
initialFlowState = FlowState
    { _stFunctions = empty
    , _stBlocks = empty
    , _stInstructions = []
    , _stVariables = (empty, [])
    , _stVariableCounter = 0
    , _stBlockCounter = 0
    , _stFunctionCounter = 0
    , _stBlockReferences = initialBlockRefs
    }

-- Starts a function.
startFunction :: State FlowState FlowState
startFunction = do
    -- Fetch old state.
    oldState <- get

    -- Set blocks, instructions and block references to initial state.
    let innerState
            = (stBlocks ^= empty)
            . (stInstructions ^= []) 
            . (stBlockReferences ^= initialBlockRefs)
            $ oldState
    put innerState
    
    return oldState

-- Finishes a function.
finishFunction :: FlowState -> BlockReference -> BlockReference -> [Variable] -> Variable -> State FlowState FunctionReference
finishFunction oldState entryBlockRef exitBlockRef params retVar = do
    -- Fetch new state.
    newState <- get
    
    -- Create function.
    let function = Function
            { flow = newState ^. stBlocks
            , entry = entryBlockRef
            , exit = exitBlockRef
            , params = params
            , returnVar = retVar
            }

    -- Add function.
    let thisFunction = newState ^. stFunctionCounter

    -- Merge states.
    let nextState
            -- Recover old blocks, instructions, and block references.
            = (stBlocks ^= (oldState ^. stBlocks))
            . (stInstructions ^= (oldState ^. stInstructions))
            . (stBlockReferences ^= (oldState ^. stBlockReferences))
            -- Insert new function.
            . (stFunctions ^%= insert thisFunction function)
            . (stFunctionCounter ^%= succ)
            $ newState

    put nextState
    
    -- Return reference.
    return thisFunction

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState ()
appendInstruction instr = modify (stInstructions ^%= (++[instr]))

-- Gets a new block reference.
getBlockReference :: State FlowState BlockReference
getBlockReference = do
    s <- get
    put $ (stBlockCounter ^%= succ) s
    return (s ^. stBlockCounter)

-- Gets block reference.
getBlockReferences :: State FlowState BlockReferences
getBlockReferences = do
    s <- get
    return $ s ^. stBlockReferences

-- Sets block reference.
setBlockReferences :: BlockReferences -> State FlowState ()
setBlockReferences blockRefs = modify (stBlockReferences ^= blockRefs)

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
    s <- get
    let (_, exitBlockRef, breakBlockRef) = s ^. stBlockReferences
    let newBlockRefs = (blockRef, exitBlockRef, breakBlockRef)

    put $ (stInstructions ^= []) . (stBlockReferences ^= newBlockRefs) $ s

-- Finishes a block. Returns its reference.
finishBlock :: FlowInstruction -> State FlowState ()
finishBlock flowInstr = do
    s <- get
    let (currentBlockRef, exitBlockRef, breakBlockRef) = s ^. stBlockReferences

    let block = Block (s ^. stInstructions) flowInstr
    let newBlockRefs = (unavailableBlockRef, exitBlockRef, breakBlockRef)
    
    put $ (stBlocks ^%= insert currentBlockRef block)
        . (stInstructions ^= [])
        . (stBlockReferences ^= newBlockRefs)
        $ s

-- Starts a variable scope.
startScope :: State FlowState ()
startScope = do
    s <- get
    let (globals, scoped) = s ^. stVariables 
    put $ stVariables ^= (globals, empty : scoped) $ s

-- Ends a scope.
endScope :: State FlowState ()
endScope = do
    s <- get
    let (globals, _ : scoped) = s ^. stVariables
    put $ stVariables ^= (globals, scoped) $ s

-- Gets a variable by name. The variable will be created as a global it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    s <- get
    let (globals, scoped) = s ^. stVariables

    case catMaybes . map (lookup name) $ scoped ++ [globals] of
        (var:_) -> return var
        []      -> do
            let newVar = Variable name
            put $ stVariables ^= (insert name newVar globals, scoped) $ s
            return newVar

-- Creates a local variable.
createVariable :: String -> State FlowState Variable
createVariable name = do
    s <- get
    let (globals, (thisScope:deeperScopes)) = s ^. stVariables

    let newVarName = '%' : show (s ^. stVariableCounter)
    let newVar     = Variable newVarName
    put $ (stVariables ^= (globals, insert name newVar thisScope : deeperScopes))
        . (stVariableCounter ^%= succ)
        $ s
    
    return newVar

-- Gets a new unique variable name.
getNewVariable :: State FlowState Variable
getNewVariable = do
    s <- get
    let (globals, (thisScope:deeperScopes)) = s ^. stVariables

    let newVarName = '%' : show (s ^. stVariableCounter)
    let newVar     = Variable newVarName
    put $ (stVariables ^= (globals, insert newVarName newVar thisScope : deeperScopes))
        . (stVariableCounter ^%= succ)
        $ s
    
    return newVar
