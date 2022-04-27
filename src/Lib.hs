module Lib where

import Tree

import Data.Map (Map)
import qualified Data.Map as M
import Data.Heap ()
import qualified Data.Heap as H
import Data.List ( unfoldr )


{- Create Huffman Tree -}

type FreqTable a = Map a Int

createFreqTable :: Ord a => [a] -> FreqTable a
createFreqTable = foldr func M.empty
  where
    func x = M.insertWith (+) x 1

createQueue :: Ord a => [a] -> H.Heap (WTree a)
createQueue = M.foldrWithKey func H.empty . createFreqTable
  where
    func k v = H.insert (createWTree v k)

_buildTree :: H.Heap (WTree a) -> Maybe (Tree a)
_buildTree pq
    | H.null pq  = Nothing
    | H.null pq' = Just (H.payload min1)
    | otherwise  = _buildTree (H.insert (mergeWTrees min1 min2) pq'')
  where
    min1 = H.minimum pq
    pq'  = H.deleteMin pq

    min2 = H.minimum pq'
    pq'' = H.deleteMin pq'

buildTree :: Ord a => [a] -> Maybe (Tree a)
buildTree = _buildTree . createQueue

{- Create Encoding -}

type Encoding = [Bool]

createEncodingTable :: Ord a => Tree a -> Map a Encoding
createEncodingTable t = dfs t []
  where
    dfs (Leaf a)     enc = M.singleton a (reverse enc)
    dfs (Node t1 t2) enc = dfs t1 (False : enc) <>
                           dfs t2 (True  : enc)

findEncoding :: Ord a => Map a Encoding -> a -> Maybe Encoding
findEncoding = flip M.lookup

encodeAll :: Ord a => Tree a -> [a] -> Maybe Encoding
encodeAll t xs = concat <$> mapM (findEncoding et) xs
  where
    et = createEncodingTable t

{- Decode Encoding -}

decodeTree :: Tree a -> Encoding -> Maybe (a, Encoding)
decodeTree (Leaf a)     ds     = Just (a, ds)
decodeTree (Node t1 t2) (d:ds) = if d then decodeTree t2 ds else decodeTree t1 ds
decodeTree (Node _ _)   []     = Nothing

decodeAll :: Tree a -> Encoding -> Maybe [a]
decodeAll (Leaf _) _ = Nothing
decodeAll t      enc = Just $ unfoldr (decodeTree t) enc