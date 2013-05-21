{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import LUAnalyse.Parser.Parser

main :: IO ()
main =  do ast <- parseLuaFile "test.lua"
           print ast
           return ()
