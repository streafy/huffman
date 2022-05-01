module Decode where

import Lib  ( Encoding, decodeAll )
import Tree ( Tree )

import System.IO (withFile, IOMode (ReadMode, WriteMode))
import qualified Data.ByteString.Lazy as BL
import Data.Bits ( Bits(shiftL, (.|.), testBit) )
import Data.Binary ( Word8, decode ) 
import Data.Maybe ( fromMaybe )


toEncoding :: Word8 -> Encoding
toEncoding w = reverse $ [testBit w i | i <- [0..7]]

toEncodingList :: [Word8] -> Encoding
toEncodingList = concatMap toEncoding

toInt :: BL.ByteString -> Int
toInt = BL.foldl f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile input output =
    withFile input  ReadMode  $ \hIn ->
    withFile output WriteMode $ \hOut -> do
        lenBS <- BL.hGet hIn 8
        let len = toInt lenBS

        treeBS <- BL.hGet hIn len
        let tree = decode treeBS :: Tree Word8

        encLenBS <- BL.hGet hIn 8
        let encLen = toInt encLenBS

        encBS <- BL.hGetContents hIn
        let enc = take encLen $ toEncodingList (BL.unpack encBS)
        
        let decoded = fromMaybe (error "Empty decoded") (decodeAll tree enc)
        BL.hPut hOut (BL.pack decoded)