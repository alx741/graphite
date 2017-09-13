{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Graph where

import Data.List (foldl')

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
type Graph v e = HM.HashMap v (Links v e)

-- | The Empty (order-zero) 'Graph' with no vertices and no edges
empty :: (Hashable v) => Graph v e
empty = HM.empty

-- | @O(log n)@ Insert a vertex into a 'Graph'
-- | If the graph already contains the vertex leave the graph untouched
insertVertex :: (Hashable v, Eq v) => v -> Graph v e -> Graph v e
insertVertex v = hashMapInsert v HM.empty

-- | @O(n)@ Remove a vertex from a 'Graph' if present
-- | Every 'Edge' adjacent to this vertex is also removed
-- FIXME
-- removeVertex :: (Hashable v, Eq v) => v -> Graph v e -> Graph v e
-- removeVertex v g = HM.delete v $ foldl' (flip removeArc) g $ incidentArcs g v

-- | @O(m*log n)@ Insert a many vertices into a 'Graph'
-- | New vertices are inserted and already contained vertices are left untouched
insertVertices :: (Hashable v, Eq v) => [v] -> Graph v e -> Graph v e
insertVertices vs g = foldl' (flip insertVertex) g vs
