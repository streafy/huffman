{-# LANGUAGE DeriveGeneric #-}
module Tree where

import GHC.Generics
import Data.Binary
import Data.Heap ( Entry(Entry) ) 

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Generic)

instance Binary a => Binary (Tree a)

createTree :: a -> Tree a
createTree = Leaf 

mergeTrees :: Tree a -> Tree a -> Tree a
mergeTrees = Node

type WTree a = Entry Int (Tree a)

createWTree :: Int -> a -> WTree a
createWTree w  = Entry w . createTree

mergeWTrees :: WTree a -> WTree a -> WTree a
mergeWTrees (Entry w1 t1) (Entry w2 t2) = Entry (w1 + w2) (mergeTrees t1 t2)