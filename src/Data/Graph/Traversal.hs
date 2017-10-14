module Data.Graph.Traversal where

import Data.Graph.Types

-- | breadh-first-search vertices starting at a particular vertex
bfsVertices :: Graph g => g v e -> v -> [v]
bfsVertices g v = tripleDestVertex <$> bfsEdges g v

-- | Same as 'bfsVertices' but gives back the connecting edges
bfsEdges :: Graph g => g v e -> v -> [(v, v, e)]
bfsEdges = undefined

-- | depth-first-search vertices starting at a particular vertex
dfsVertices :: Graph g => g v e -> v -> [v]
dfsVertices g v = tripleDestVertex <$> dfsEdges g v

-- | Same as 'dfsVertices' but gives back the connecting edges
dfsEdges :: Graph g => g v e -> v -> [(v, v, e)]
dfsEdges = undefined
