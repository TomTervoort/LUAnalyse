{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Generator(generateControlFlow) where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)

import LUAnalyse.ControlFlow.Flow
import qualified LUAnalyse.Parser.AST as Ast

-- Flow generation state.
type FlowState = (Map BlockReference Block, [Instruction], Map String Variable, Int, BlockReference, BlockReference)

-- Starting state.
initialFlowState :: FlowState
initialFlowState = (empty, [], empty, 0, 0, -1)

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState Variable
appendInstruction instr = do
    modify $ \ (blocks, instructions, variables, varCounter, blockCounter, blockRef) ->
        (blocks, instructions ++ [instr], variables, varCounter, blockCounter, blockRef)
    return $ var instr

-- Gets a new block reference.
getBlockReference :: State FlowState BlockReference
getBlockReference = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRef) <- get
    put (blocks, instructions, variables, varCounter, blockCounter + 1, blockRef)
    return blockCounter

-- Starts a block. Returns its reference.
startBlock :: BlockReference -> State FlowState BlockReference
startBlock blockRef = do
    (blocks, instructions, variables, varCounter, blockCounter, _) <- get
    put (blocks, instructions, variables, varCounter, blockCounter, blockRef)
    return blockRef

-- Finishes a block. Returns its reference.
finishBlock :: FlowInstruction -> State FlowState BlockReference
finishBlock flowInstr = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRef) <- get
    block <- return $ Block instructions flowInstr
    put (insert blockRef block blocks, [], variables, varCounter, blockCounter, -1)
    return blockCounter

-- Creates a variable alias.
createVariableAlias :: String -> Variable -> State FlowState Variable
createVariableAlias name var = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRef) <- get
    put (blocks, instructions, insert name var variables, varCounter, blockCounter, blockRef)
    return var

-- Gets a variable by name. The variable will be created if it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    (blocks, instructions, variables, varCounter, blockCounter, blockRef) <- get
    case lookup name variables of
        Just var -> do return var
        Nothing -> do
            put (blocks, instructions, newVariables, varCounter, blockCounter, blockRef)
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
    (blocks, instructions, variables, varCounter, blockCounter, blockRef) <- get
    put (blocks, instructions, variables, varCounter + 1, blockCounter, blockRef)
    getVariable $ "%" ++ show varCounter

-- Generates a control flow from an AST.
generateControlFlow :: Ast.AST -> Flow
generateControlFlow ast = flow
    where
        (flow, _, _, _, _, _) = execState (handleFunction ast) initialFlowState

-- Handles a function.
handleFunction :: Ast.Block -> State FlowState ()
handleFunction block = do
    entryBlockRef <- getBlockReference
    startBlock entryBlockRef
    handleBlock block
    finishBlock ReturnInstr
    return ()

-- Handles a block.
handleBlock :: Ast.Block -> State FlowState ()
handleBlock (Ast.StatList statements) = do
    mapM_ handleStatement statements

-- Handles a statement.
handleStatement :: Ast.Statement -> State FlowState ()
handleStatement statement =
    case statement of
        Ast.AssignmentStatement {Ast.lhs = lhs, Ast.rhs = rhs} -> handleAssignments lhs rhs
        
        Ast.CallStatement  {Ast.expr = expr} -> do
            handleExpr expr
            return ()
        
        Ast.IfStatement {Ast.condition = condition, Ast.thenBody = thenBlock, Ast.elseBody = elseBody} ->
            handleIf condition thenBlock elseBody
            
                    
                
                
        
        -- Ast.CallStatement {Ast.expr = expr}
        -- Ast.LocalStatement {locals :: [Name], inits :: [Expr]}
        -- Ast.WhileStatement {condition :: Expr, body :: Block}
        -- Ast.DoStatement {body :: Block}
        -- Ast.ReturnStatement {args :: [Expr]}
        -- Ast.BreakStatement
        -- Ast.RepeatStatement {body :: Block, condition :: Expr}
        -- Ast.FunctionDecl {isLocal :: Bool, name :: Name, argList :: [Name], body :: Block}
        -- Ast.GenericForStatement {vars :: [Name], generators :: [Expr], body :: Block}
        -- Ast.NumericForStatement {var :: Name, start :: Expr, end :: Expr, step :: Maybe Expr, body :: Block}


-- Handles if.
handleIf :: Ast.Expr -> Ast.Block -> Maybe Ast.Block -> State FlowState ()
handleIf condition thenBlock elseBody = case elseBody of
    Just elseBlock -> do
        condVar <- handleExpr condition
        
        thenBlockRef <- getBlockReference
        elseBlockRef <- getBlockReference
        endBlockRef  <- getBlockReference
        
        finishBlock CondJumpInstr {target = thenBlockRef, alternative = elseBlockRef, cond = condVar}
        
        startBlock thenBlockRef
        handleBlock thenBlock
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock elseBlockRef
        handleBlock elseBlock
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock endBlockRef
        
        return ()
        
    Nothing -> do
        condVar <- handleExpr condition
        
        thenBlockRef <- getBlockReference
        endBlockRef  <- getBlockReference
        
        finishBlock CondJumpInstr {target = thenBlockRef, alternative = endBlockRef, cond = condVar}
        
        startBlock thenBlockRef
        handleBlock thenBlock
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock endBlockRef
        
        return ()

-- Handles assignments.
handleAssignments :: [Ast.Expr] -> [Ast.Expr] -> State FlowState ()
handleAssignments lhs rhs = mapM_ (uncurry handleAssignment) $ zip lhs rhs

-- Handles an assignment.
handleAssignment :: Ast.Expr -> Ast.Expr -> State FlowState Variable
handleAssignment lhs rhs =
    case lhs of
        Ast.VarExpr (Ast.Name name) -> do
            rhsVar <- handleExpr rhs
            createVariableAlias name rhsVar
            
            return rhsVar
        Ast.MemberExpr expr (Ast.Name member) -> do
            rhsVar  <- handleExpr rhs
            exprVar <- handleExpr expr
            
            appendInstruction $ NewMemberInstr {var = exprVar, member = Name member, value = rhsVar}
            return rhsVar
        Ast.IndexExpr expr index -> do
            rhsVar   <- handleExpr rhs
            exprVar  <- handleExpr expr
            indexVar <- handleExpr index
            
            appendInstruction $ NewIndexInstr {var = exprVar, index = indexVar, value = rhsVar}
            return rhsVar

-- Handles expressions.
handleExpr :: Ast.Expr -> State FlowState Variable
handleExpr expr = 
    case expr of
        -- Variable.
        Ast.VarExpr (Ast.Name name) -> do
            var <- lookupVariable name
            case var of
                Just var -> return var
                Nothing  -> handleConstant NilConst
        
        -- Constants.
        Ast.NumberExpr double -> handleConstant $ NumberConst double
        Ast.StringExpr str    -> handleConstant $ StringConst str
        Ast.BooleanExpr bool  -> handleConstant $ BooleanConst bool
        Ast.NilExpr           -> handleConstant NilConst
        
        -- Operators.
        Ast.BinopExpr first op second -> handleBinaryOperator first op second
        Ast.UnopExpr op expr          -> handleUnaryOperator op expr
        
        -- Indexing.
        Ast.IndexExpr expr index -> do
            exprVar  <- handleExpr expr
            indexVar <- handleExpr index
            var      <- getNewVariable
            
            appendInstruction $ IndexInstr {var = var, value = exprVar, index = indexVar}
            return var
        Ast.MemberExpr expr (Ast.Name member) -> do
            exprVar  <- handleExpr expr
            var      <- getNewVariable
            
            appendInstruction $ MemberInstr {var = var, value = exprVar, member = Name member}
            return var
        
        -- Calls.
        Ast.CallExpr method arguments -> do
            methodVar <- handleExpr method
            argVars   <- mapM handleExpr arguments
            var       <- getNewVariable
            
            appendInstruction $ CallInstr {var = var, method = methodVar, args = argVars}
            return var
            
        -- Ast.DotsExpr
        -- Ast.FunctionExpr [Name] Block
        -- Ast.ConstructorExpr [(String, Expr)]

-- Handles binary operators.
handleBinaryOperator :: Ast.Expr -> Ast.Operator -> Ast.Expr -> State FlowState Variable
handleBinaryOperator first (Ast.Operator op) second = do
    firstExpr  <- handleExpr first
    secondExpr <- handleExpr second
    var        <- getNewVariable
    
    appendInstruction $ opInstruction var firstExpr secondExpr
    return var
        where
            opInstruction var firstExpr secondExpr = case op of
                "+" -> AddInstr {var = var, first = firstExpr, second = secondExpr}
                "-" -> SubInstr {var = var, first = firstExpr, second = secondExpr}
                "*" -> MulInstr {var = var, first = firstExpr, second = secondExpr}
                "/" -> DivInstr {var = var, first = firstExpr, second = secondExpr}
                "%" -> ModInstr {var = var, first = firstExpr, second = secondExpr}
                "^" -> PowInstr {var = var, first = firstExpr, second = secondExpr}
                
                "==" -> EqInstr        {var = var, first = firstExpr, second = secondExpr}
                "~=" -> NotEqInstr     {var = var, first = firstExpr, second = secondExpr}
                "<"  -> LessInstr      {var = var, first = firstExpr, second = secondExpr}
                ">"  -> GreaterInstr   {var = var, first = firstExpr, second = secondExpr}
                "<=" -> LessEqInstr    {var = var, first = firstExpr, second = secondExpr}
                ">=" -> GreaterEqInstr {var = var, first = firstExpr, second = secondExpr}
                
                -- TODO: and / or

-- Handles unary operators.
handleUnaryOperator :: Ast.Operator -> Ast.Expr -> State FlowState Variable
handleUnaryOperator (Ast.Operator op) expr = do
    exprVar <- handleExpr expr
    var     <- getNewVariable
    
    appendInstruction $ opInstruction var exprVar
    return var
        where
            opInstruction var exprVar = case op of
                "-"   -> MinusInstr {var = var, value = exprVar}
                "not" -> NotInstr   {var = var, value = exprVar}

-- Handles constants.
handleConstant :: Constant -> State FlowState Variable
handleConstant constant = do
    var <- getNewVariable
    appendInstruction $ ConstInstr {var = var, constant = constant}
    return var

