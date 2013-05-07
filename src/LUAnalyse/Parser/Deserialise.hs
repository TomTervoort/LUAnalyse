{-# LANGUAGE Haskell2010, ScopedTypeVariables #-}

-- | Converts an abstract syntax tree from out serialised intermediate representation to the 
--   corresponding AST datatype.
module LUAnalyse.Parser.Deserialise (readAST) where

import LUAnalyse.Parser.AST

import Data.Generics
import Data.List
import Control.Arrow
import Control.Monad.State
import Debug.Trace
import Data.Char

readAST :: String -> AST
readAST = readData . trimTrailingSpaces
 where trimTrailingSpaces = reverse . dropWhile isSpace . reverse

splitCtorString :: String -> (String, [String])
splitCtorString ('(':cs) = ("", splitArgs 0 False $ init cs)
  where splitArgs 0 False []       = []
        splitArgs 0 False (',':cs) = "" : splitArgs 0 False cs
        splitArgs p False ('(':cs) = '('  `ins` splitArgs (p + 1) False cs
        splitArgs p False (')':cs) = ')'  `ins` splitArgs (p - 1) False cs
        splitArgs p q ('\'':cs)    = '\'' `ins` splitArgs p (not q) cs
        splitArgs p q ( c :cs)     =  c   `ins` splitArgs p q cs
        splitArgs _ _ _ = error "Parse error in serialised AST."

        ins x []     = [[x]]
        ins x (y:ys) = (x:y) : ys
splitCtorString ( c :cs) = first (c:) $ splitCtorString cs
splitCtorString _ = error "Parse error in serialised AST."

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
       stringInp = unescape $ init $ tail $ inp
       unescape []          = []
       unescape ('\'':c:cs) = c : unescape cs
       unescape (c:cs)      = c : unescape cs
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

