{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Hashable

type VertexHash = Int

class Edge e where
    vertex1 :: e -> VertexHash
    vertex2 :: e -> VertexHash

class Edge a => Arc a where
    fromVertex :: a -> VertexHash
    toVertex :: a -> VertexHash

class Edge e => WeightedEdge e where
    edgeWeight :: e -> Double

instance Hashable v => Edge (v, v) where
    vertex1 = hash . fst
    vertex2 = hash . snd

instance Hashable v => Arc (v, v) where
    fromVertex = vertex1
    toVertex = vertex2
