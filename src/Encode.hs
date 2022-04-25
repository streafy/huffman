module Encode where

import Lib
import Tree ( Tree )

import qualified Data.ByteString as B
import Data.Word ( Word8 )
import Data.Bits ( Bits(shiftL, (.|.)) )
import Data.Bool ( bool )
import Data.Maybe

toWord8 :: Encoding -> Word8
toWord8 = foldl ((. bool 0 1) . (.|.) . (`shiftL` 1)) 0

toWord8List :: Encoding -> [Word8]
toWord8List [] = []
toWord8List xs
    | null ys = [toWord8 (y ++ replicate (8 - length y) False)]
    | otherwise = toWord8 y : toWord8List ys
    where (y, ys) = splitAt 8 xs

encodeFile :: FilePath -> FilePath -> IO ()
encodeFile input output = do
    content <- B.readFile input
    let tree = buildTree $ B.unpack content
    let len  = length $ B.unpack content
    let enc = encodeAll (fromJust tree) (B.unpack content)
    B.writeFile output (B.pack (toWord8List (fromJust enc)))

f 5 = qweqwe + 5