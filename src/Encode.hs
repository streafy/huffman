module Encode where

import Lib  ( Encoding, encodeAll, buildTree )
import Tree ( Tree )

import System.IO (withFile, IOMode (ReadMode, WriteMode))
import qualified Data.ByteString.Lazy as BL
import Data.Bits ( Bits(shiftL, (.|.)) )
import Data.Binary ( Word8, encode )
import Data.Maybe ( fromMaybe )
import Data.Bool ( bool )


toWord8 :: Encoding -> Word8
toWord8 = foldl f 0
  where
    f a b = a `shiftL` 1 .|. bool 0 1 b

toWord8List :: Encoding -> [Word8]
toWord8List [] = []
toWord8List xs
    | null ys = [toWord8 (y ++ replicate (8 - length y) False)]
    | otherwise = toWord8 y : toWord8List ys
    where (y, ys) = splitAt 8 xs

encodeFile :: FilePath -> FilePath -> IO ()
encodeFile input output =
    withFile input  ReadMode  $ \hIn ->
    withFile output WriteMode $ \hOut -> do
        contentBS <- BL.hGetContents hIn
        let content = BL.unpack contentBS

        let tree = fromMaybe (error "Empty tree") (buildTree content)

        let len = length $ BL.unpack $ encode tree
        BL.hPut hOut $ encode (len, tree)

        let enc = fromMaybe (error "Empty encoding") (encodeAll tree content)
        BL.hPut hOut $ encode $ length enc
        BL.hPut hOut $ BL.pack $ toWord8List enc
