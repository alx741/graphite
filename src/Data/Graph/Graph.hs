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
-- removeVertex v g = HM.delete v $ foldl' (flip removeEdge) g $ incidentEdges g v

-- | @O(m*log n)@ Insert a many vertices into a 'Graph'
-- | New vertices are inserted and already contained vertices are left untouched
insertVertices :: (Hashable v, Eq v) => [v] -> Graph v e -> Graph v e
insertVertices vs g = foldl' (flip insertVertex) g vs

-- | @O(log n)@ Insert an undirected 'Edge' into a 'Graph'
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Edge, its attribute is updated
insertEdge :: (Hashable v, Eq v) => Edge v e -> Graph v e -> Graph v e
insertEdge (Edge v1 v2 edgeAttr) g = link v2 v1 $ link v1 v2 g'
    where
        g' = insertVertices [v1, v2] g
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | @O(m*log n)@ Insert many directed 'Edge's into a 'Graph'
-- | Same rules as 'insertEdge' are applied
-- insertEdges :: (Hashable v, Eq v) => [Edge v e] -> Graph v e -> Graph v e
-- insertEdges as g = foldl' (flip insertEdge) g as


-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink = HM.insert

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> Graph v e -> Links v e
getLinks = HM.lookupDefault HM.empty
