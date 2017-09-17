-- | For Connectivity analisis purposes a 'DGraph' can be converted into a
-- | 'UGraph' using 'toUndirected'

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Connectivity where

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
                (search (directlyReachableVertices g v) banned' v')
                || (search vs banned' v')
            where banned' = v `S.insert` banned

-- | Tell if two vertices of a 'UGraph' are disconnected
-- | Two vertices are @disconnected@ if it doesn't exist a path between them
areDisconnected :: UGraph v e -> v -> v -> Bool
areDisconnected = undefined

-- | Retrieve all the unreachable vertices of a 'UGraph'
-- | The @unreachable vertices@ are those with no adjacent 'Edge's
unreachableVertices :: UGraph v e -> [v]
unreachableVertices = undefined

-- | Tell if a 'UGraph' is connected
-- | An Undirected Graph is @connected@ when there is a path between every pair
-- | of vertices
isConnected :: UGraph v e -> Bool
isConnected = undefined

-- | Tell if a 'UGraph' is disconnected
-- | An Undirected Graph is @disconnected@ when its not @connected@. See
-- | 'isConnected'
-- TODO: An edgeles graph with two or more vertices is disconnected
isDisconnected :: UGraph v e -> Bool
isDisconnected = not . isConnected

-- | Tell if a 'DGraph' is weakly connected
-- | A Directed Graph is @weakly connected@ if the equivalent undirected graph
-- | is @connected@
isWeaklyConnected :: DGraph v e -> Bool
isWeaklyConnected = undefined -- isConnected . toUndirected

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
