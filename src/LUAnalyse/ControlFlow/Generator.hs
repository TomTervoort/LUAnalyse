{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Generator(generateControlFlow) where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Lens.Common
import Data.Map hiding (map, member)

import LUAnalyse.ControlFlow.Flow
import LUAnalyse.ControlFlow.State
import qualified LUAnalyse.Parser.AST as Ast

varToAstExpr :: Variable -> Ast.Expr
varToAstExpr (Variable x) = Ast.VarExpr . Ast.Name $ x

-- Generates a control flow from an AST.
generateControlFlow :: Ast.AST -> Program
generateControlFlow ast
  = let (start, result) = runState (handleFunction [] ast) initialFlowState
    in Program {functions = result ^. stFunctions, start = start}

-- Handles a function.
handleFunction :: [Ast.Name] -> Ast.Block -> State FlowState FunctionReference
handleFunction paramNames block = do
    oldState <- startFunction
    (entryBlockRef, exitBlockRef, params, retVar) <- scoped $ do
        entryBlockRef <- getBlockReference
        exitBlockRef  <- getBlockReference
        
        retVar <- createVariable "%retval"
        
        oldExitBlockReference <- getExitBlockReference
        setExitBlockReference exitBlockRef
        
        params <- mapM (\(Ast.Name name) -> createVariable name) paramNames
        
        startBlock entryBlockRef
        nilVar <- handleConstant NilConst
        appendInstruction $ AssignInstr {var = retVar, value = nilVar}
        handleBlock block
        finishBlock JumpInstr {target = exitBlockRef}
        
        setExitBlockReference oldExitBlockReference
        startBlock exitBlockRef
        finishBlock ReturnInstr

        return (entryBlockRef, exitBlockRef, params, retVar)

    finishFunction oldState entryBlockRef exitBlockRef params retVar

-- Handles a block.
handleBlock :: Ast.Block -> State FlowState ()
handleBlock (Ast.StatList statements) = do
    mapM_ handleStatement statements

-- Handles a statement.
handleStatement :: Ast.Statement -> State FlowState ()
handleStatement statement =
    case statement of
        Ast.AssignmentStatement {Ast.lhs = lhs, Ast.rhs = rhs} ->
            handleAssignments lhs rhs
        
        Ast.CallStatement  {Ast.expr = expr} -> do
            handleExpr expr
            return ()
        
        Ast.IfStatement {Ast.condition = condition, Ast.thenBody = thenBlock, Ast.elseBody = elseBody} ->
            handleIf condition thenBlock elseBody
            
        Ast.WhileStatement {Ast.condition = condition, Ast.body = bodyBlock} ->
            handleWhile condition bodyBlock
            
        Ast.RepeatStatement {Ast.body = bodyBlock, Ast.condition = condition} ->
            handleRepeat bodyBlock condition 
            
        Ast.DoStatement {Ast.body = bodyBlock} ->
            scoped (handleBlock bodyBlock)
        
        Ast.ReturnStatement {Ast.args = args} ->
            handleReturn args
            
        Ast.BreakStatement ->
            handleBreak
        
        Ast.LocalStatement {Ast.locals = locals, Ast.inits = inits} ->
            handleLocals locals inits
            
        -- Ast.FunctionDecl {isLocal :: Bool, name :: Name, argList :: [Name], body :: Block}
        -- Ast.GenericForStatement {vars :: [Name], generators :: [Expr], body :: Block}
        -- Ast.NumericForStatement {var :: Name, start :: Expr, end :: Expr, step :: Maybe Expr, body :: Block}

-- Handles return.
handleReturn :: [Ast.Expr] -> State FlowState ()
handleReturn exprs = do
    case exprs of
        [expr] -> do
                      exprVar <- handleExpr expr
                      var     <- getVariable "%retval"
                      
                      appendInstruction $ AssignInstr {var = var, value = exprVar}
        []     -> return ()
    
    exitBlockReference <- getExitBlockReference
    junkBlockRef <- getBlockReference
    finishBlock JumpInstr {target = exitBlockReference}
    startBlock junkBlockRef
    return ()

-- Handles break.
handleBreak :: State FlowState ()
handleBreak = do
    breakBlockReference <- getBreakBlockReference
    if breakBlockReference == unavailableBlockRef
        then error "Break used outside a loop control structure."
        else do
            junkBlockRef <- getBlockReference
            finishBlock JumpInstr {target = breakBlockReference}
            startBlock junkBlockRef
            return ()

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
        scoped (handleBlock thenBlock)
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock elseBlockRef
        scoped (handleBlock elseBlock)
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock endBlockRef
        
        return ()
        
    Nothing -> do
        condVar <- handleExpr condition
        
        thenBlockRef <- getBlockReference
        endBlockRef  <- getBlockReference
        
        finishBlock CondJumpInstr {target = thenBlockRef, alternative = endBlockRef, cond = condVar}
        
        startBlock thenBlockRef
        scoped (handleBlock thenBlock)
        finishBlock JumpInstr {target = endBlockRef}
        
        startBlock endBlockRef
        
        return ()

-- Handles while.
handleWhile :: Ast.Expr -> Ast.Block -> State FlowState ()
handleWhile condition bodyBlock = do
    condBlockRef <- getBlockReference
    bodyBlockRef <- getBlockReference
    endBlockRef  <- getBlockReference
    
    finishBlock JumpInstr {target = condBlockRef}
    
    oldBreakBlockReference <- getBreakBlockReference
    setBreakBlockReference endBlockRef
    
    startBlock condBlockRef
    condVar <- handleExpr condition
    finishBlock CondJumpInstr {target = bodyBlockRef, alternative = endBlockRef, cond = condVar}
    
    startBlock bodyBlockRef
    scoped (handleBlock bodyBlock)
    finishBlock JumpInstr {target = condBlockRef}
    
    setBreakBlockReference oldBreakBlockReference
    startBlock endBlockRef
    
    return ()

-- Handles repeat.
handleRepeat :: Ast.Block -> Ast.Expr -> State FlowState ()
handleRepeat bodyBlock condition = do
    bodyBlockRef <- getBlockReference
    endBlockRef  <- getBlockReference
    
    finishBlock JumpInstr {target = bodyBlockRef}
    
    oldBreakBlockReference <- getBreakBlockReference
    setBreakBlockReference endBlockRef
    
    startBlock bodyBlockRef
    condVar <- scoped $ do
        handleBlock bodyBlock
        handleExpr condition
    finishBlock CondJumpInstr {target = endBlockRef, alternative = bodyBlockRef, cond = condVar}
    
    setBreakBlockReference oldBreakBlockReference
    startBlock endBlockRef
    
    return ()

-- Handle local assignments.
handleLocals :: [Ast.Name] -> [Ast.Expr] -> State FlowState ()
handleLocals locals inits = do
    localVars <- mapM (\(Ast.Name name) -> createVariable name) locals
    
    mapM_ (uncurry handleAssignment) $ zip (map varToAstExpr localVars) inits

-- Handles assignments.
handleAssignments :: [Ast.Expr] -> [Ast.Expr] -> State FlowState ()
handleAssignments [lhs] [rhs] = do
    handleAssignment lhs rhs
    return ()

handleAssignments lhs rhs = do
    exprVars <- mapM handleExpr rhs
    
    tempVars <- mapM assignTempVar exprVars
    
    mapM_ (uncurry handleAssignment) $ zip lhs (map varToAstExpr tempVars)
        where
            assignTempVar var = do
                tempVar <- getNewVariable
                
                appendInstruction $ AssignInstr {var = tempVar, value = var}
                
                return tempVar

-- Handles an assignment.
handleAssignment :: Ast.Expr -> Ast.Expr -> State FlowState Variable
handleAssignment lhs rhs =
    case lhs of
        Ast.VarExpr (Ast.Name name) -> do
            rhsVar <- handleExpr rhs
            var    <- getVariable name
            
            appendInstruction $ AssignInstr {var = var, value = rhsVar}
            
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
            getVariable name
            -- var <- lookupVariable name
            -- case var of
            --    Just var -> return var
            --    Nothing  -> handleConstant NilConst -- TODO: Forward references.
        
        -- Constants.
        Ast.NumberExpr double -> handleConstant $ NumberConst double
        Ast.StringExpr str    -> handleConstant $ StringConst str
        Ast.BooleanExpr bool  -> handleConstant $ BooleanConst bool
        Ast.NilExpr           -> handleConstant NilConst
        
        -- Table constructor.
        Ast.ConstructorExpr pairs -> handleConstructor pairs
        
        -- Operators.
        Ast.BinopExpr first (Ast.Operator "and") second -> handleAndOperator first second
        Ast.BinopExpr first (Ast.Operator "or") second  -> handleOrOperator first second
        
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
        
        -- Closure.
        Ast.FunctionExpr paramNames block -> do
            functionRef <- handleFunction paramNames block
            handleConstant $ FunctionConst functionRef

-- Handles and operator.
handleAndOperator :: Ast.Expr -> Ast.Expr -> State FlowState Variable
handleAndOperator first second = do
    -- Get block references.
    firstBlockRef  <- getBlockReference
    secondBlockRef <- getBlockReference
    endBlockRef    <- getBlockReference
    
    -- Check if first value is true.
    var      <- getNewVariable
    firstVar <- handleExpr first
    finishBlock CondJumpInstr {target = firstBlockRef, alternative = secondBlockRef, cond = firstVar}
    
    -- If so, set to second value.
    startBlock firstBlockRef
    secondVar <- handleExpr second
    appendInstruction $ AssignInstr {var = var, value = secondVar}
    finishBlock JumpInstr {target = endBlockRef}
    
    -- Otherwise, set to first value.
    startBlock secondBlockRef
    appendInstruction $ AssignInstr {var = var, value = firstVar}
    finishBlock JumpInstr {target = endBlockRef}
    
    -- Start end block.
    startBlock endBlockRef
    
    return var

-- Handles or operator.
handleOrOperator :: Ast.Expr -> Ast.Expr -> State FlowState Variable
handleOrOperator first second = do
    -- Get block references.
    firstBlockRef  <- getBlockReference
    secondBlockRef <- getBlockReference
    endBlockRef    <- getBlockReference
    
    -- Check if first value is false.
    var      <- getNewVariable
    firstVar <- handleExpr first
    finishBlock CondJumpInstr {target = secondBlockRef, alternative = firstBlockRef, cond = firstVar}
    
    -- If so, set to second value.
    startBlock firstBlockRef
    secondVar <- handleExpr second
    appendInstruction $ AssignInstr {var = var, value = secondVar}
    finishBlock JumpInstr {target = endBlockRef}
    
    -- Otherwise, set to first value.
    startBlock secondBlockRef
    appendInstruction $ AssignInstr {var = var, value = firstVar}
    finishBlock JumpInstr {target = endBlockRef}
    
    -- Start end block.
    startBlock endBlockRef
    
    return var

-- Handles binary operators.
handleBinaryOperator :: Ast.Expr -> Ast.Operator -> Ast.Expr -> State FlowState Variable
handleBinaryOperator first (Ast.Operator op) second = do
    firstExpr  <- handleExpr first
    secondExpr <- handleExpr second
    var'       <- getNewVariable
    
    appendInstruction $ opInstruction var' firstExpr secondExpr
    return var'
        where
            opInstruction var' firstExpr secondExpr = case op of
                "+" -> AddInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                "-" -> SubInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                "*" -> MulInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                "/" -> DivInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                "^" -> PowInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                "%" -> ModInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                
                ".." -> ConcatInstr {var = var', lhs = firstExpr, rhs = secondExpr}
                
                "==" -> EqInstr        {var = var', lhs = firstExpr, rhs = secondExpr}
                "~=" -> NotEqInstr     {var = var', lhs = firstExpr, rhs = secondExpr}
                "<"  -> LessInstr      {var = var', lhs = firstExpr, rhs = secondExpr}
                ">"  -> GreaterInstr   {var = var', lhs = firstExpr, rhs = secondExpr}
                "<=" -> LessEqInstr    {var = var', lhs = firstExpr, rhs = secondExpr}
                ">=" -> GreaterEqInstr {var = var', lhs = firstExpr, rhs = secondExpr}

-- Handles unary operators.
handleUnaryOperator :: Ast.Operator -> Ast.Expr -> State FlowState Variable
handleUnaryOperator (Ast.Operator op) expr = do
    exprVar <- handleExpr expr
    var'    <- getNewVariable
    
    appendInstruction $ opInstruction var' exprVar
    return var'
        where
            opInstruction var' exprVar = case op of
                "-"   -> MinusInstr  {var = var', value = exprVar}
                "not" -> NotInstr    {var = var', value = exprVar}
                "#"   -> LengthInstr {var = var', value = exprVar}

-- Handles constants.
handleConstant :: Constant -> State FlowState Variable
handleConstant constant = do
    var' <- getNewVariable
    appendInstruction $ ConstInstr {var = var', constant = constant}
    return var'

-- Handles table construction.
handleConstructor :: [(Ast.Expr, Ast.Expr)] -> State FlowState Variable
handleConstructor pairs = do
    var' <- getNewVariable
    appendInstruction $ ConstInstr {var = var', constant = TableConst}
    
    mapM_ (\(key, v) -> handleAssignment (Ast.IndexExpr (varToAstExpr var') key) v) pairs
    
    return var'

-- Post-processing:
-- - Set constant flag for every instruction.
-- - Create member instrs from value instrs
