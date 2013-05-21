{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Generator(generateControlFlow) where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map hiding (map, member)

import LUAnalyse.ControlFlow.Flow
import qualified LUAnalyse.Parser.AST as Ast

-- Flow generation state.
type FlowState = (Map BlockReference Block, [Instruction], Map String Variable, Int, BlockReference)

-- Starting state.
initialFlowState :: FlowState
initialFlowState = (empty, [], empty, 0, 0)

-- Appends an instruction.
appendInstruction :: Instruction -> State FlowState Variable
appendInstruction instr = do
    modify $ \ (blocks, instructions, variables, varCounter, blockCounter) ->
        (blocks, instructions ++ [instr], variables, varCounter, blockCounter)
    return $ var instr

-- Gets current block reference.
getBlockReference :: State FlowState BlockReference
getBlockReference = do
    (_, _, _, _, blockCounter) <- get
    return blockCounter

-- Appends a block. Returns its reference.
appendBlock :: FlowInstruction -> State FlowState BlockReference
appendBlock flowInstr = do
    (blocks, instructions, variables, varCounter, blockCounter) <- get
    block <- return $ Block instructions flowInstr
    put (insert blockCounter block blocks, [], variables, varCounter, blockCounter + 1)
    return blockCounter

-- Creates a variable alias.
createVariableAlias :: String -> Variable -> State FlowState Variable
createVariableAlias name var = do
    (blocks, instructions, variables, varCounter, blockCounter) <- get
    put (blocks, instructions, insert name var variables, varCounter, blockCounter)
    return var

-- Gets a variable by name. The variable will be created if it does not yet exist.
getVariable :: String -> State FlowState Variable
getVariable name = do
    (blocks, instructions, variables, varCounter, blockCounter) <- get
    case lookup name variables of
        Just var -> do return var
        Nothing -> do
            put (blocks, instructions, newVariables, varCounter, blockCounter)
            return newVar
                where
                    newVariables = insert name newVar variables
                    newVar       = Variable name

-- Looks up a variable by name. An error will occur if the variable does not exist.
lookupVariable :: String -> State FlowState Variable
lookupVariable name = do
    (_, _, variables, _, _) <- get
    case lookup name variables of
        Just var -> return var
        Nothing -> error $ "Variable named '" ++ name ++ "' does not exist."

-- Gets a new unique variable name.
getNewVariable :: State FlowState Variable
getNewVariable = do
    (blocks, instructions, variables, varCounter, blockCounter) <- get
    put (blocks, instructions, variables, varCounter + 1, blockCounter)
    getVariable $ "%" ++ show varCounter

-- Generates a control flow from an AST.
generateControlFlow :: Ast.AST -> Flow
generateControlFlow ast = flow
    where
        (flow, _, _, _, _) = execState (handleBlock ast) initialFlowState

-- Handles a block.
handleBlock :: Ast.Block -> State FlowState ()
handleBlock (Ast.StatList statements) = do
    mapM_ handleStatement statements
    appendBlock ReturnInstr
    return ()

-- Handles a statement.
handleStatement :: Ast.Statement -> State FlowState ()
handleStatement statement =
    case statement of
        Ast.AssignmentStatement {Ast.lhs = lhs, Ast.rhs = rhs} -> handleAssignments lhs rhs

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
        Ast.VarExpr (Ast.Name name) -> lookupVariable name
        
        -- Constants.
        Ast.NumberExpr double -> handleConstant $ NumberConst double
        Ast.StringExpr str    -> handleConstant $ StringConst str
        Ast.BooleanExpr bool  -> handleConstant $ BooleanConst bool
        Ast.NilExpr           -> handleConstant NilConst
        
        -- Operators.
        Ast.BinopExpr first op second -> handleBinaryOperator first op second
        Ast.UnopExpr op expr          -> handleUnaryOperator op expr
        
        -- Ast.DotsExpr
        -- Ast.CallExpr Expr [Expr]
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
                
                -- TODO: && / ||

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


{-




        
        -- LocalStatement {locals = locals, inits = inits} ->
        -- CallStatement {exp :: Expr} -- TODO: How is call a statement? It is an expression.
        
        {-
        LocalStatement {locals :: [Name],  inits :: [Expr]}
        IfStatement {condition :: Expr, thenBody ::  Block, elseBody :: Maybe Block}
        WhileStatement {condition :: Expr, body :: Block}
        DoStatement {body :: Block}
        ReturnStatement {args :: [Expr]}
        BreakStatement
        RepeatStatement {body :: Block, condition :: Expr}
        FunctionDecl {isLocal :: Bool, name :: Name, argList :: [Name], body :: Block} -- TODO?: method, varargs
        GenericForStatement {vars :: [Name], generators :: [Expr], body :: Block}
        NumericForStatement {var :: Name, start :: Expr, end :: Expr, step :: Maybe Expr, body :: Block}
        -}




-- Handles a single assignment.
handleAssignment :: [Ast.Expr] -> [Ast.Expr] -> State FlowState Variable
handleAssignment lhs rhs = case lhs of
    Ast.VarExpr (Ast.Name name) ->
        rhsExpr ++ [AssignInstr {var = Variable name, value = var rhsExpr}]
            where
                rhsExpr = handleExpr rhs
            
    Ast.MemberExpr expr member ->
        rhsExpr ++ exprExpr ++ [NewMemberInstr {var = var exprExpr, member = member, value = var rhsExpr}]
            where
                exprExpr = handleExpr expr
                rhsExpr  = handleExpr rhs
            
    Ast.IndexExpr expr index ->
        rhsExpr ++ exprExpr ++ indexExpr ++ [NewIndexInstr {var = var exprExpr, index = var indexExpr, value = var rhsExpr}]
            where
                exprExpr = handleExpr expr
                indexExpr = handleExpr expr
                rhsExpr  = handleExpr rhs
-}





-- StatList [
-- LocalStatement {locals = [Name "x"], inits = [NumberExpr 5.0]},
-- NumericForStatement {var = Name "i", start = NumberExpr 0.0, end = VarExpr (Name "x"), step = Nothing, body = StatList [CallStatement {exp = CallExpr (VarExpr (Name "print")) [VarExpr (Name "i")]}]}]

