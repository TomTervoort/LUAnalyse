{-# LANGUAGE Haskell2010, DeriveDataTypeable, ScopedTypeVariables #-}

module AST where

import Data.Generics
import Data.List
import Control.Arrow
import Control.Monad.State
import Debug.Trace


data Block = StatList [Statement]
              deriving (Typeable, Data, Show)

data Statement = AssignmentStatement {lhs :: [Expr], rhs :: [Expr]}
               | CallStatement {exp :: Expr}
               | LocalStatement {locals :: [Name],  inits :: [Expr]}
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
          | ConstructorExpr [(String, Expr)]
             deriving (Typeable, Data, Show)

newtype Operator = Operator String deriving (Typeable, Data, Show)

newtype Name = Name String deriving (Typeable, Data, Show)


readAST :: String -> Block
readAST inp = readData inp 

splitCtorString :: String -> (String, [String])
splitCtorString ('(':cs) = ("", splitArgs 0 $ init cs)
  where splitArgs 0 []       = []
        splitArgs 0 (',':cs) = "" : splitArgs 0 cs
        splitArgs p ('(':cs) = '(' `ins` splitArgs (p + 1) cs
        splitArgs p (')':cs) = ')' `ins` splitArgs (p - 1) cs
        splitArgs p ( c :cs) = c   `ins` splitArgs p cs
        ins x []     = [[x]]
        ins x (y:ys) = (x:y) : ys
splitCtorString ( c :cs) = first (c:) $ splitCtorString cs

getConstrByName :: String -> DataType -> Maybe Constr
getConstrByName name ty = fmap (indexConstr ty . (+1)) 
                             $ findIndex (== name) 
                             $ map showConstr
                             $ dataTypeConstrs ty

readData :: forall a. Data a => String -> a
readData inp = dataInp `extB` stringInp           `extB` numInp              
                       `extB` nameInp             `extB` (listInp :: [Statement])  
                       `extB` (listInp :: [Expr]) `extB` (listInp :: [Name])

 where ty :: DataType
       ty = dataTypeOf (undefined :: a)
       stringInp :: String
       stringInp = read inp
       numInp :: Double
       numInp = read inp
       nameInp :: Name
       nameInp = Name inp
       listInp :: forall b. Data b => [b]
       listInp = case splitCtorString inp of
                  ("List", contents) -> map readData contents
                  (ctor, _)          -> error $ "Expecting List, got '" ++ ctor ++ "'."

       dataInp :: a
       dataInp = let (ctor, args) = splitCtorString inp
                  in case getConstrByName ctor ty of
                      Nothing -> error $ "Type '" ++ dataTypeName ty 
                                                  ++ "' has no constructor named '"
                                                  ++ ctor ++ "'."
                      Just c  -> evalState (fromConstrM readArgs c) args
       readArgs :: forall b. Data b => State [String] b
       readArgs = state $ \(x:xs) -> (readData x, xs)

