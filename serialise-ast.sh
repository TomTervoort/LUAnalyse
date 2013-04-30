#!/bin/bash

# Lelijk shellscriptje dat SerialiseAST.lua uitvoert.

cd LuaMinify
lua SerialiseAST.lua ../$1
cd ..