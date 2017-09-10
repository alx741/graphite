{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.Hashable

type VertexHash = Int

class Edge e where
    vertices :: e  -> (VertexHash, VertexHash)

class Edge a => Arc a where
    fromVertex :: a -> VertexHash
    toVertex :: a -> VertexHash

class Edge e => WeightedEdge e where
    edgeWeight :: e -> Double

instance Hashable v => Edge (v, v) where
    vertices (v1, v2) = (hash v1, hash v2)

instance Hashable v => Arc (v, v) where
    fromVertex = fst . vertices
    toVertex = snd . vertices
