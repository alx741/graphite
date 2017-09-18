-- | For Connectivity analisis purposes a 'DGraph' can be converted into a
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
-- | Two vertices are @connected@ if it exists a path between them
areConnected :: forall g v e . (Graph g, Hashable v, Eq v, Ord v)
 => g v e
 -> v
 -> v
 -> Bool
areConnected g fromV toV
    | fromV == toV = True
    | otherwise = search (directlyReachableVertices g fromV) S.empty toV
    where
        search :: [v] -> S.Set v -> v -> Bool
        search [] _ _ = False
        search (v:vs) banned v'
            | v `S.member` banned = search vs banned v'
            | v == v' = True
            | otherwise =
                search (directlyReachableVertices g v) banned' v'
                || search vs banned' v'
            where banned' = v `S.insert` banned

-- | Tell if two vertices of a 'UGraph' are disconnected
-- | Two vertices are @disconnected@ if it doesn't exist a path between them
areDisconnected :: (Graph g, Hashable v, Eq v, Ord v) => g v e -> v -> v -> Bool
areDisconnected g fromV toV = not $ areConnected g fromV toV

-- | Tell if a vertex is isolated
-- | A vertex is @isolated@ if it has no incidet edges, that is, it has a degree
-- | of zero
isIsolated :: (Graph g, Hashable v, Eq v) => g v e -> v -> Bool
isIsolated g v = vertexDegree g v == 0

-- | Tell if a graph is connected
-- | An Undirected Graph is @connected@ when there is a path between every pair
-- | of vertices
isConnected :: (Hashable v, Eq v) => UGraph v e -> Bool
isConnected g = foldl' (\b v -> b && (not $ isIsolated g v)) True $ vertices g

-- | Tell if a 'DGraph' is weakly connected
-- | A Directed Graph is @weakly connected@ if the underlying undirected graph
-- | is @connected@
isWeaklyConnected :: DGraph v e -> Bool
isWeaklyConnected = undefined -- isConnected . toUndirected

-- | Tell if a 'DGraph' is strongly connected
-- | A Directed Graph is @strongly connected@ if it contains a directed path
-- | on every pair of vertices in both directions
isStronglyConnected :: DGraph v e -> Bool
isStronglyConnected = undefined -- isConnected . toUndirected

-- | Retrieve all the unreachable vertices of a 'UGraph'
-- | The @unreachable vertices@ are those with no adjacent 'Edge's
unreachableVertices :: UGraph v e -> [v]
unreachableVertices = undefined

-- TODO
-- * connected component
-- * strong components
-- * vertex cut
-- * vertex connectivity
--     * biconnectivity
--     * triconnectivity
--     * separable
-- * bridge
-- * edge-connectivity
-- * maximally connected
-- * maximally edge-connected
-- * super-connectivity
-- * hyper-connectivity
-- * Menger's theorem

-- Robin's Theorem: a graph is orientable if it is connected and has no bridges
