{-# LANGUAGE Haskell2010 #-}

-- | Run the Lua parser (written in Lua) on a source file and transform its output into the AST 
--   datatype.
--
--   NOTE: make sure lua 5.1 (not >= 5.2!) is installed and executable through the command 'lua'.
module LUAnalyse.Parser (parseLuaFile) where

import LUAnalyse.Parser.AST
import LUAnalyse.Parser.Deserialise

import System.Directory
import System.Process
import System.IO

parseLuaFile :: FilePath -> IO AST
parseLuaFile file = 
 do absPath <- canonicalizePath file
    let parseProc = (proc "lua" ["SerialiseAST.lua", absPath])
                      {cwd = Just "src/LUAnalyse/Parser/LuaParser", 
                       std_err = UseHandle stderr,
                       std_out = CreatePipe}
    (_, Just out, _, ph) <- createProcess parseProc
    hSetBinaryMode out False
    outStr <- hGetContents out
    return $ readAST outStr