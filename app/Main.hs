module Main where

import Tree
import Lib

import System.Environment

import qualified Streaming.ByteString.Char8 as Q


main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then error "Incorrent arguments" else print args