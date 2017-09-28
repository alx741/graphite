module Data.Graph.Morphisms where

import Data.Graph.Types

-- | Tell if two graphs are isomorphic
-- TODO: check first: same number of vertices, same number of edges
areIsomorphic :: Graph g => g v e -> g v' e' -> Bool
areIsomorphic = undefined

isomorphism :: Graph g => g v e -> g v' e' -> (v -> v')
isomorphism = undefined
