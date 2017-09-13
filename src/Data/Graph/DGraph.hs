{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DGraph where

import Data.List (foldl')

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

import Data.Graph.Types

-- | Directed Graph of Vertices in /v/ and Arcs with attributes in /e/
newtype DGraph v e = DGraph { unDGraph :: HM.HashMap v (Links v e) }
    deriving (Eq, Show)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (DGraph v e) where
    arbitrary = insertArcs <$> arbitrary <*> pure empty

-- | The Degree Sequence un a 'DGraph' is a list of pairs (Indegree, Outdegree)
type DegreeSequence = [(Int, Int)]

-- | The Empty (order-zero) 'DGraph' with no vertices and no arcs
empty :: (Hashable v) => DGraph v e
empty = DGraph HM.empty

-- | @O(log n)@ Insert a vertex into a 'DGraph'
-- | If the graph already contains the vertex leave the graph untouched
insertVertex :: (Hashable v, Eq v) => v -> DGraph v e -> DGraph v e
insertVertex v (DGraph g) = DGraph $ hashMapInsert v HM.empty g

-- | @O(n)@ Remove a vertex from a 'DGraph' if present
-- | Every 'Arc' incident to this vertex is also removed
removeVertex :: (Hashable v, Eq v) => v -> DGraph v e -> DGraph v e
removeVertex v g = DGraph
    $ (\(DGraph g') -> HM.delete v g')
    $ foldl' (flip removeArc) g $ incidentArcs g v

-- | @O(m*log n)@ Insert a many vertices into a 'DGraph'
-- | New vertices are inserted and already contained vertices are left untouched
insertVertices :: (Hashable v, Eq v) => [v] -> DGraph v e -> DGraph v e
insertVertices vs g = foldl' (flip insertVertex) g vs

-- | @O(log n)@ Insert a directed 'Arc' into a 'DGraph'
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Arc, its attribute is updated
insertArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
insertArc (Arc fromV toV edgeAttr) g = DGraph
    $ HM.adjust (insertLink toV edgeAttr) fromV g'
    where g' = unDGraph $ insertVertices [fromV, toV] g

-- | @O(m*log n)@ Insert many directed 'Arc's into a 'DGraph'
-- | Same rules as 'insertArc' are applied
insertArcs :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e -> DGraph v e
insertArcs as g = foldl' (flip insertArc) g as

-- | @O(log n)@ Remove the directed 'Arc' from a 'DGraph' if present
-- | The involved vertices are left untouched
removeArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
removeArc = removeArc' . toOrderedPair

-- | Same as 'removeArc' but the arc is an ordered tuple
removeArc' :: (Hashable v, Eq v) => (v, v) -> DGraph v e -> DGraph v e
removeArc' (v1, v2) (DGraph g) = case HM.lookup v1 g of
    Nothing -> DGraph g
    Just v1Links -> DGraph $ HM.adjust (const v1Links') v1 g
        where v1Links' = HM.delete v2 v1Links

-- | @O(log n)@ Remove the directed 'Arc' from a 'DGraph' if present
-- | The involved vertices are also removed
removeArcAndVertices :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
removeArcAndVertices = removeArcAndVertices' . toOrderedPair

-- | Same as 'removeArcAndVertices' but the arc is an ordered tuple
removeArcAndVertices' :: (Hashable v, Eq v) => (v, v) -> DGraph v e -> DGraph v e
removeArcAndVertices' (v1, v2) g =
    removeVertex v2 $ removeVertex v1 $ removeArc' (v1, v2) g

-- | @O(n)@ Retrieve the vertices of a 'DGraph'
vertices :: DGraph v e -> [v]
vertices (DGraph g) = HM.keys g

-- | @O(n)@ Retrieve the order of a 'DGraph'
-- | The @order@ of a graph is its number of vertices
order :: DGraph v e -> Int
order (DGraph g) = HM.size g

-- | @O(n*m)@ Retrieve the size of a 'DGraph'
-- | The @size@ of a directed graph is its number of 'Arc's
size :: (Hashable v, Eq v) => DGraph v e -> Int
size = length . arcs

-- | @O(n*m)@ Retrieve the 'Arc's of a 'DGraph'
arcs :: forall v e . (Hashable v, Eq v) => DGraph v e -> [Arc v e]
arcs (DGraph g) = linksToArcs $ zip vs links
    where
        vs :: [v]
        vs = vertices $ DGraph g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | Same as 'arcs' but the arcs are ordered tuples, and their attributes are
-- | discarded
arcs' :: (Hashable v, Eq v) => DGraph v e -> [(v, v)]
arcs' g = toOrderedPair <$> arcs g

-- | @O(log n)@ Tell if a vertex exists in the graph
containsVertex :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
containsVertex (DGraph g) = flip HM.member g

-- | @O(log n)@ Tell if a directed 'Arc' exists in the graph
containsArc :: (Hashable v, Eq v) => DGraph v e -> Arc v e -> Bool
containsArc g = containsArc' g . toOrderedPair

-- | Same as 'containsArc' but the arc is an ordered tuple
containsArc' :: (Hashable v, Eq v) => DGraph v e -> (v, v) -> Bool
containsArc' graph@(DGraph g) (v1, v2) =
    containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
    where v1Links = getLinks v1 g

-- | Retrieve the inbounding 'Arc's of a Vertex
inboundingArcs :: (Hashable v, Eq v) => DGraph v e -> v -> [Arc v e]
inboundingArcs g v = filter (\(Arc _ toV _) -> v == toV) $ arcs g

-- | Retrieve the outbounding 'Arc's of a Vertex
outboundingArcs :: (Hashable v, Eq v) => DGraph v e -> v -> [Arc v e]
outboundingArcs g v = filter (\(Arc fromV _ _) -> v == fromV) $ arcs g

-- | Retrieve the incident 'Arc's of a Vertex
-- | Both inbounding and outbounding arcs
incidentArcs :: (Hashable v, Eq v) => DGraph v e -> v -> [Arc v e]
incidentArcs g v = inboundingArcs g v ++ outboundingArcs g v

-- | Retrieve the adjacent vertices of a vertex
adjacentVertices :: DGraph v e -> v -> [v]
adjacentVertices = undefined

-- | Tell if a 'DGraph' is symmetric
-- | All of its 'Arc's are bidirected
isSymmetric :: DGraph v e -> Bool
isSymmetric = undefined

-- | Tell if a 'DGraph' is oriented
-- | There are none bidirected 'Arc's
-- | Note: This is /not/ the opposite of 'isSymmetric'
isOriented :: DGraph v e -> Bool
isOriented = undefined

-- | Tell if a 'DGraph' is isolated
-- | A graph is @isolated@ if it has no edges, that is, it has a degree of 0
-- | TODO: What if it has a loop?
isIsolated :: DGraph v e -> Bool
isIsolated = undefined

-- | Degree of a vertex
-- | The total number of inbounding and outbounding 'Arc's of a vertex
vertexDegree :: DGraph v e -> v -> Int
vertexDegree g v = vertexIndegree g v + vertexOutdegree g v

-- | Indegree of a vertex
-- | The number of inbounding 'Arc's to a vertex
vertexIndegree :: DGraph v e -> v -> Int
vertexIndegree = undefined

-- | Outdegree of a vertex
-- | The number of outbounding 'Arc's from a vertex
vertexOutdegree :: DGraph v e -> v -> Int
vertexOutdegree = undefined

-- | Indegrees of all the vertices in a 'DGraph'
indegrees :: DGraph v e -> [Int]
indegrees = undefined

-- | Outdegree of all the vertices in a 'DGraph'
outdegrees :: DGraph v e -> [Int]
outdegrees = undefined

-- | Tell if a 'DGraph' is balanced
-- | A Directed Graph is @balanced@ when its @indegree = outdegree@
isBalanced :: DGraph v e -> Bool
isBalanced g = sum (indegrees g) == sum (outdegrees g)

-- | Tell if a 'DGraph' is regular
-- | A Directed Graph is @regular@ when all of its vertices have the same number
-- | of adjacent vertices AND when the @indegree@ and @outdegree@ of each vertex
-- | are equal to each toher.
isRegular :: DGraph v e -> Bool
isRegular g = undefined

-- | Tell if a vertex is a source
-- | A vertex is a @source@ when its @indegree = 0@
isSource :: DGraph v e -> v -> Bool
isSource g v = vertexIndegree g v == 0

-- | Tell if a vertex is a sink
-- | A vertex is a @sink@ when its @outdegree = 0@
isSink :: DGraph v e -> v -> Bool
isSink g v = vertexOutdegree g v == 0

-- | Tell if a vertex is internal
-- | A vertex is a @internal@ when its neither a @source@ nor a @sink@
isInternal :: DGraph v e -> v -> Bool
isInternal g v = not $ isSource g v || isSink g v

-- | Tell if a 'DegreeSequence' is a Directed Graphic
-- | A @Directed Graphic@ is a Degree Sequence for wich a 'DGraph' exists
-- TODO: Kleitman–Wang | Fulkerson–Chen–Anstee theorem algorithms
isDirectedGraphic :: DegreeSequence -> Bool
isDirectedGraphic = undefined
