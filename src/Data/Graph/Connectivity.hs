-- | For Connectivity analisis purposes a 'DGraph' can be converted into a
-- | 'Graph' using 'toUndirected'

module Data.Graph.Connectivity where

import Data.Graph.Graph
import Data.Graph.DGraph

-- | Tell if a 'Graph' is connected
-- | An Undirected Graph is @connected@ when there is a path between every pair
-- | of vertices
isConnected :: Graph v e -> Bool
isConnected = undefined

-- | Tell if a 'Graph' is disconnected
-- | An Undirected Graph is @disconnected@ when its not @connected@. See
-- | 'isConnected'
-- TODO: An edgeles graph with two or more vertices is disconnected
isDisconnected :: Graph v e -> Bool
isDisconnected = not . isConnected

-- | Tell if two vertices of a 'Graph' are connected
-- | Two vertices are @connected@ if it exists a path between them
areConnected :: Graph v e -> v -> v -> Bool
areConnected = undefined

-- | Tell if two vertices of a 'Graph' are disconnected
-- | Two vertices are @disconnected@ if it doesn't exist a path between them
areDisconnected :: Graph v e -> v -> v -> Bool
areDisconnected = undefined

-- | Retrieve all the unreachable vertices of a 'Graph'
-- | The @unreachable vertices@ are those with no adjacent 'Edge's
unreachableVertices :: Graph v e -> [v]
unreachableVertices = undefined

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
