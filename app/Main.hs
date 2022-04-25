module Main where

import Tree
import Lib
import Encode ( encodeFile )

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
    case args of
        ["encode", input, output] -> encodeFile input output
        ["decode", input, output] -> undefined 
        _                         -> putStrLn "incorrect input"