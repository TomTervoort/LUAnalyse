{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import LUAnalyse.Parser.Parser
import LUAnalyse.ControlFlow.Generator
import LUAnalyse.ControlFlow.Formatter
-- import LUAnalyse.ControlFlow.Resolver
import LUAnalyse.Framework.Framework
import LUAnalyse.Analysis.Constants

import LUAnalyse.Analysis.SoftTyping.Analysis

{-

import Control.Arrow
import Data.Maybe
import Data.Map (Map)-}
import qualified Data.Map as M
{-import Data.Set (Set)
import qualified Data.Set as S

-}

main :: IO ()
main =  do [filename] <- getArgs
           ast <- parseLuaFile filename
           print ast
           program <- return $ generateControlFlow ast
           putStrLn "\n"
           putStrLn $ formatControlFlow program
           
           -- print $ nextInstructions program Nothing
           -- print $ edges program ForwardAnalysis

           putStrLn "\n--------\n\n"
           
           let constants = performConstantsAnalysis program
           forM_ (M.toList constants) $ \(lbl, stls) -> putStrLn $ show lbl ++ ": " ++ show stls
           
           putStrLn "\n--------\n\n"
           
           let foo = performAnalysis SoftTypingAnalysis program
           forM_ (M.toList foo) $ \(lbl, stls) -> putStrLn $ show lbl ++ ": " ++ show stls

           -- print $ M.unions $ map flow $ M.elems $ functions program -- $ M.assocs $ M.unions $ map flow $ M.elems $ (functions program) -- 
           -- print $ labelInstructions program
           return ()
