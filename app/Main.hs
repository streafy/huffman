module Main where

import Tree
import Encoder
import System.Environment

import Data.Heap
import qualified Data.Heap as Heap

main :: IO ()
main = do
    args <- getArgs
    let (input, output) = case args of
                               i:o:_ -> (i, o)
                               _     -> error "No input or output file"
    print args
    print input
    print output