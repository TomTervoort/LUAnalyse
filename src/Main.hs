{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import LUAnalyse.Parser.Parser
import LUAnalyse.ControlFlow.Generator
import LUAnalyse.ControlFlow.Formatter
-- import LUAnalyse.ControlFlow.Resolver
import LUAnalyse.Framework.Framework
import LUAnalyse.Analysis.Constants

{-

import Control.Arrow
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-}

main :: IO ()
main =  do ast <- parseLuaFile "test.lua"
           print ast
           program <- return $ generateControlFlow ast
           putStrLn "\n"
           putStrLn $ formatControlFlow program
           
           -- print $ nextInstructions program Nothing
           -- print $ edges program ForwardAnalysis
           
           constants <- return $ performConstantsAnalysis program
           print constants
           
           -- print $ M.unions $ map flow $ M.elems $ functions program -- $ M.assocs $ M.unions $ map flow $ M.elems $ (functions program) -- 
           -- print $ labelInstructions program
           return ()
