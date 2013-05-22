{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

module LUAnalyse.Parser.AST where

import Data.Data

type AST = Block

data Block = StatList [Statement]
              deriving (Typeable, Data, Show)

data Statement = AssignmentStatement {lhs :: [Expr], rhs :: [Expr]}
               | CallStatement {expr :: Expr}
               | LocalStatement {locals :: [Name], inits :: [Expr]}
               | IfStatement {condition :: Expr, thenBody ::  Block, elseBody :: Maybe Block}
               | WhileStatement {condition :: Expr, body :: Block}
               | DoStatement {body :: Block}
               | ReturnStatement {args :: [Expr]}
               | BreakStatement
               | RepeatStatement {body :: Block, condition :: Expr}
               | FunctionDecl {isLocal :: Bool, name :: Name, argList :: [Name], body :: Block} -- TODO?: method, varargs
               | GenericForStatement {vars :: [Name], generators :: [Expr], body :: Block}
               | NumericForStatement {var :: Name, start :: Expr, end :: Expr, step :: Maybe Expr, body :: Block}
                  deriving (Typeable, Data, Show)

data Expr = VarExpr Name
          | NumberExpr Double
          | StringExpr String
          | BooleanExpr Bool
          | NilExpr
          | BinopExpr Expr Operator Expr
          | UnopExpr Operator Expr
          | DotsExpr
          | CallExpr Expr [Expr]
          | IndexExpr Expr Expr
          | MemberExpr Expr Name
          | FunctionExpr [Name] Block
          | ConstructorExpr [(Expr, Expr)]
             deriving (Typeable, Data, Show)

newtype Operator = Operator String deriving (Typeable, Data, Show)

newtype Name = Name String deriving (Typeable, Data, Show)
