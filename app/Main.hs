module Main where

import Tree
import Lib
import Encode ( encodeFile )
import Decode ( decodeFile )

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["encode", input, output] -> encodeFile input output
        ["decode", input, output] -> decodeFile input output 
        _                         -> putStrLn "Incorrect input"