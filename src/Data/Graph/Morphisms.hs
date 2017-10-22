module Data.Graph.Morphisms where

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Tell if two graphs are isomorphic
-- TODO: check first: same number of vertices, same number of edges
areIsomorphic :: Graph g => g v e -> g v' e' -> Bool
areIsomorphic = undefined

isomorphism :: Graph g => g v e -> g v' e' -> (v -> v')
isomorphism = undefined

-- | Tell if a 'UGraph' is regular
--
-- An undirected graph is @regular@ if each vertex has the same degree
isURegular :: UGraph v e -> Bool
isURegular = undefined

-- | Tell if a 'DGraph' is regular
--
-- A directed graph is @regular@ if each vertex has the same indegree and
-- outdegree
isDRegular :: DGraph v e -> Bool
isDRegular = undefined
