{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Converts an abstract syntax tree from out serialised intermediate representation to the 
--   corresponding AST datatype.
module LUAnalyse.Parser.Deserialise (readAST) where

import LUAnalyse.Parser.AST

import Data.Generics
import Data.List
import Control.Arrow
import Control.Monad.State
import Data.Char

readAST :: String -> AST
readAST = readData . trimTrailingSpaces
 where trimTrailingSpaces = reverse . dropWhile isSpace . reverse

splitCtorString :: String -> (String, [String])
splitCtorString ('(':cz) = ("", splitArgs 0 False $ init cz)
  where splitArgs :: Int -> Bool -> String -> [String]
        splitArgs 0 False []       = []
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
getConstrByName nm ty = fmap (indexConstr ty . (+1)) 
                             $ findIndex (== nm)
                             $ map showConstr
                             $ dataTypeConstrs ty

readData :: forall r. Data r => String -> r
readData inp = dataInp `extB` stringInp                 `extB` numInp              
                       `extB` boolInp                   `extB` nameInp
                       `extB` (pairInp :: (Expr, Expr)) `extB` (listInp :: [Statement])
                       `extB` (listInp :: [Expr])       `extB` (listInp :: [Name])
                       `extB` (listInp :: [(Expr, Expr)])

 where ty :: DataType
       ty = dataTypeOf (undefined :: r)
       
       stringInp :: String
       stringInp = unescape . init . tail $ inp
       
       unescape []          = []
       unescape ('\'':c:cs) = c : unescape cs
       unescape (c:cs)      = c : unescape cs
       
       numInp :: Double
       numInp = read inp
       
       boolInp :: Bool
       boolInp = read inp
       
       nameInp :: Name
       nameInp = Name inp
       
       pairInp :: forall a b. (Data a, Data b) => (a, b)
       pairInp = case splitCtorString inp of
                  ("Pair", [ll, rr]) -> (readData ll, readData rr)
                  (ctor, _)          -> error $ "Expecting pair, got '" ++ ctor ++ "'."
       
       listInp :: forall b. Data b => [b]
       listInp = case splitCtorString inp of
                  ("List", contents) -> map readData contents
                  (ctor, _)          -> error $ "Expecting list, got '" ++ ctor ++ "'."

       dataInp :: r
       dataInp = let (ctor, parameters) = splitCtorString inp
                  in case getConstrByName ctor ty of
                      Nothing -> error $ "Type '" ++ dataTypeName ty 
                                                  ++ "' has no constructor named '"
                                                  ++ ctor ++ "'."
                      Just c  -> evalState (fromConstrM readArgs c) parameters
       
       readArgs :: forall b. Data b => State [String] b
       readArgs = state $ \(x:xs) -> (readData x, xs)

