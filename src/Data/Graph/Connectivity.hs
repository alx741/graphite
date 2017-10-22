-- | For Connectivity analysis purposes a 'DGraph' can be converted into a
-- | 'UGraph' using 'toUndirected'

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Connectivity where

import Data.List (foldl')

import           Data.Hashable
import qualified Data.Set      as S

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Tell if two vertices of a graph are connected
--
-- Two vertices are @connected@ if it exists a path between them. The order of
-- the vertices is relevant when the graph is directed
areConnected :: forall g v e . (Graph g, Hashable v, Eq v, Ord v)
 => g v e
 -> v
 -> v
 -> Bool
areConnected g fromV toV
    | fromV == toV = True
    | otherwise = search (fromV : reachableAdjacentVertices g fromV) S.empty toV
    where
        search :: [v] -> S.Set v -> v -> Bool
        search [] _ _ = False
        search (v:vs) banned v'
            | v `S.member` banned = search vs banned v'
            | v == v' = True
            | otherwise =
                search (v : reachableAdjacentVertices g v) banned' v'
                || search vs banned' v'
            where banned' = v `S.insert` banned

-- | Opposite of 'areConnected'
areDisconnected :: (Graph g, Hashable v, Eq v, Ord v) => g v e -> v -> v -> Bool
areDisconnected g fromV toV = not $ areConnected g fromV toV

-- | Tell if a vertex is isolated
--
-- A vertex is @isolated@ if it has no incident edges, that is, it has a degree
-- of zero
isIsolated :: (Graph g, Hashable v, Eq v) => g v e -> v -> Bool
isIsolated g v = vertexDegree g v == 0

-- | Tell if a graph is connected
--
-- An undirected graph is @connected@ when there is a path between every pair
-- of vertices
-- FIXME: Use a O(n) algorithm
isConnected :: (Graph g, Hashable v, Eq v, Ord v) => g v e -> Bool
isConnected g = go vs True
    where
        vs = vertices g
        go _ False = False
        go [] bool = bool
        go (v':vs') bool =
            go vs' $ foldl' (\b v -> b && areConnected g v v') bool vs

-- | Tell if a graph is bridgeless
--
-- A graph is @bridgeless@ if it has no edges that, when removed, split the
-- graph in two isolated components
-- FIXME: Use a O(n) algorithm
isBridgeless :: (Hashable v, Eq v, Ord v) => UGraph v e -> Bool
isBridgeless g =
    foldl' (\b vs -> b && isConnected (removeEdgePair vs g)) True (edgePairs g)

-- | Tell if a 'UGraph' is orientable
--
-- An undirected graph is @orientable@ if it can be converted into a directed
-- graph that is @strongly connected@ (See 'isStronglyConnected')
isOrientable :: (Hashable v, Eq v, Ord v) => UGraph v e -> Bool
isOrientable g = isConnected g && isBridgeless g

-- | Tell if a 'DGraph' is weakly connected
--
-- A directed graph is @weakly connected@ if the underlying undirected graph
-- is @connected@
isWeaklyConnected :: (Hashable v, Eq v, Ord v) => DGraph v e -> Bool
isWeaklyConnected = isConnected . toUndirected

-- | Tell if a 'DGraph' is strongly connected
--
-- A directed graph is @strongly connected@ if it contains a directed path
-- on every pair of vertices
isStronglyConnected :: (Hashable v, Eq v, Ord v) => DGraph v e -> Bool
isStronglyConnected = isConnected

-- TODO
-- connected component
-- strong components
-- vertex cut
-- vertex connectivity
--   biconnectivity
--   triconnectivity
--   separable
-- bridge
-- edge-connectivity
-- maximally connected
-- maximally edge-connected
-- super-connectivity
-- hyper-connectivity
-- Menger's theorem

-- Robin's Theorem: a graph is orientable if it is connected and has no bridges
