{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import LUAnalyse.Parser.Parser
import LUAnalyse.ControlFlow.Generator
import LUAnalyse.ControlFlow.Formatter
-- import LUAnalyse.ControlFlow.Resolver
import LUAnalyse.Analysis.Constants

main :: IO ()
main =  do ast <- parseLuaFile "test.lua"
           print ast
           flow <- return $ generateControlFlow ast
           putStrLn "\n"
           putStrLn $ formatControlFlow flow
           
           constants <- return $ performConstantsAnalysis flow
           print constants
           return ()
