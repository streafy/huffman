module Encoder where

import Tree

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Heap ()
import qualified Data.Heap as Heap

import Data.Maybe

type FreqTable a = Map a Int

data Direction = DLeft
               | DRight
               deriving (Show, Eq)

type Encoding = [Direction]

createFreqTable :: Ord a => [a] -> FreqTable a
createFreqTable = foldr func Map.empty
  where
    func x = Map.insertWith (+) x 1

createQueue :: Ord a => [a] -> Heap.Heap (WTree a)
createQueue = Map.foldrWithKey func Heap.empty . createFreqTable
  where
    func k v = Heap.insert (createWTree v k)

_buildTree :: Heap.Heap (WTree a) -> Maybe (Tree a)
_buildTree pq
    | Heap.null pq  = Nothing
    | Heap.null pq' = Just (Heap.payload min1)
    | otherwise     = _buildTree (Heap.insert (mergeWTrees min1 min2) pq'')
  where
    min1 = Heap.minimum pq
    pq'  = Heap.deleteMin pq

    min2 = Heap.minimum pq'
    pq'' = Heap.deleteMin pq'    

buildTree :: Ord a => [a] -> Maybe (Tree a)
buildTree = _buildTree . createQueue

createEncodingTable :: Ord a => Tree a -> Map a Encoding
createEncodingTable t = dfs t []
  where
    dfs (Leaf a) enc = Map.singleton a (reverse enc)
    dfs (Node t1 t2) enc = dfs t1 (DLeft : enc) <>
                           dfs t2 (DRight : enc)

findEncoding :: Ord a => Map a Encoding -> a -> Maybe Encoding
findEncoding = flip Map.lookup

test = createEncodingTable $ fromJust $ buildTree "hello world"