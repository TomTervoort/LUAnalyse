{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import LUAnalyse.Parser.Parser
import LUAnalyse.ControlFlow.Generator
import LUAnalyse.ControlFlow.Formatter

main :: IO ()
main =  do ast <- parseLuaFile "test.lua"
           print ast
           flow <- return $ generateControlFlow ast
           putStrLn "\n"
           putStrLn $ formatControlFlow flow
           return ()
