{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DiGraph where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

import Data.Graph.Types

myGraph :: DiGraph Int (Double, String)
myGraph = insertArc (Arc 1 2 (55.5, "label")) empty


-- | Directed Graph of Vertices in /v/ and Arcs with attributes in /e/
type DiGraph v e = HM.HashMap v (Links v e)

-- | The Empty (order-zero) 'DiGraph' with no vertices and no arcs
empty :: (Hashable v) => DiGraph v e
empty = HM.empty

-- | @O(log n)@ Insert a vertex into a 'DiGraph'
-- | If the graph already contains the vertex, leave the graph untouched
insertVertex :: (Hashable v, Eq v) => v -> DiGraph v e -> DiGraph v e
insertVertex v = hashMapInsert v HM.empty

-- | @O(m*log n)@ Insert a many vertices into a 'DiGraph'
-- | New vertices are inserted and already contained vertices are left untouched
insertVertices :: (Hashable v, Eq v) => [v] -> DiGraph v e -> DiGraph v e
insertVertices vs g = foldl (flip insertVertex) g vs

-- | @O(log n)@ Insert a directed 'Arc' into a 'DiGraph'
-- | The implied vertices are inserted if don't exist. If the graph already
-- | contains the Arc, its attribute is updated
insertArc :: (Hashable v, Eq v) => Arc v e -> DiGraph v e -> DiGraph v e
insertArc (Arc fromV toV edgeAttr) g = HM.adjust (insertLink toV edgeAttr) fromV g'
    where g' = insertVertices [fromV, toV] g

-- | @O(m*log n)@ Insert many directed 'Arc's into a 'DiGraph'
-- | Same rules as 'insertArc' are applied
insertArcs :: (Hashable v, Eq v) => [Arc v e] -> DiGraph v e -> DiGraph v e
insertArcs as g = foldl (flip insertArc) g as

-- | Retrieve the vertices of a 'DiGraph'
vertices :: DiGraph v e -> [v]
vertices = HM.keys

-- | Retrieve the 'Arc's of a 'DiGraph'
arcs :: forall v e . (Hashable v, Eq v, Eq e) => DiGraph v e -> [Arc v e]
arcs g = linksToArcs $ zip vs links
    where
        vs :: [v]
        vs = vertices g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | Retrieve the 'Arc's of a 'DiGraph' as tuples, ignoring its attribute values
arcs' :: (Hashable v, Eq v, Eq e) => DiGraph v e -> [(v, v)]
arcs' g = arcToTuple <$> arcs g

-- | Retrieve the incident 'Arc's of a Vertex
incidentArcs :: DiGraph v e -> v -> [Arc v e]
incidentArcs = undefined

-- | Retrieve the adjacent vertices of a vertex
adjacentVertices :: DiGraph v e -> v -> [v]
adjacentVertices = undefined

-- | Tell if a 'DiGraph' is symmetric
-- | All of its 'Arc's are bidirected
isSymmetric :: DiGraph v e -> Bool
isSymmetric = undefined

-- | Tell if a 'DiGraph' is oriented
-- | There are none bidirected 'Arc's
-- | Note: This is /not/ the opposite of 'isSymmetric'
isOriented :: DiGraph v e -> Bool
isOriented = undefined

-- | Indegree of a vertex
-- | The number of inbounding adjacent 'Arc's to a vertex
vertexIndegree :: DiGraph v e -> v -> Int
vertexIndegree = undefined

-- | Outdegree of a vertex
-- | The number of outbounding adjacent 'Arc's from a vertex
vertexOutdegree :: DiGraph v e -> v -> Int
vertexOutdegree = undefined

-- | Indegree of a 'DiGraph'
-- | The total indegree of all the vertices in a 'DiGraph'
indegree :: DiGraph v e -> Int
indegree = undefined

-- | Outdegree of a 'DiGraph'
-- | The total outdegree of all the vertices in a 'DiGraph'
outdegree :: DiGraph v e -> Int
outdegree = undefined

-- | Tell if a 'DiGraph' is balanced
-- | A Directed Graph is @balanced@ when its @indegree = outdegree@
isBalanced :: DiGraph v e -> Bool
isBalanced g = indegree g == outdegree g

-- | Tell if a vertex is a source
-- | A vertex is a @source@ when its @indegree = 0@
isSource :: DiGraph v e -> v -> Bool
isSource g v = vertexIndegree g v == 0

-- | Tell if a vertex is a sink
-- | A vertex is a @sink@ when its @outdegree = 0@
isSink :: DiGraph v e -> v -> Bool
isSink g v = vertexOutdegree g v == 0

-- | Tell if a vertex is internal
-- | A vertex is a @internal@ when its neither a @source@ nor a @sink@
isInternal :: DiGraph v e -> v -> Bool
isInternal g v = not $ isSource g v || isSink g v


-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink = HM.insert

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> DiGraph v e -> Links v e
getLinks = HM.lookupDefault HM.empty
