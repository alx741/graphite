{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Types where

import Data.List    (foldl')
import GHC.Float    (float2Double)
import GHC.Generics (Generic)

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

class Graph g where
    -- | The Empty (order-zero) graph with no vertices and no edges
    empty :: (Hashable v) => g v e

    -- | Retrieve the order of a graph
    -- | The @order@ of a graph is its number of vertices
    order :: g v e -> Int

    -- | Retrieve the size of a graph
    -- | The @size@ of a graph is its number of edges
    size :: (Hashable v, Eq v) => g v e -> Int
    size = length . edgePairs

    -- | Retrieve the vertices of a graph
    vertices :: g v e -> [v]

    -- | Retrieve the edges of a graph
    edgeTriples :: (Hashable v, Eq v) => g v e -> [(v, v, e)]

    -- | Retrieve the edges of a graph, ignoring its attributes
    edgePairs :: (Hashable v, Eq v) => g v e -> [(v, v)]
    edgePairs g = tripleToPair <$> edgeTriples g

    -- | Tell if a vertex exists in the graph
    containsVertex :: (Hashable v, Eq v) => g v e -> v -> Bool

    -- | Tell if two vertices are adjacent
    areAdjacent :: (Hashable v, Eq v) => g v e -> v -> v -> Bool

    -- | Retrieve the adjacent vertices of a vertex
    adjacentVertices :: (Hashable v, Eq v) => g v e -> v -> [v]
    adjacentVertices g v = (\(_, v', _) -> v') <$> adjacentVertices' g v

    -- | Same as 'adjacentVertices' but gives back the connecting edges
    adjacentVertices' :: (Hashable v, Eq v) => g v e -> v -> [(v, v, e)]

    -- | Same as 'adjacentVertices' but gives back only those vertices for which
    -- | the connecting edge allows the vertex to be reached.
    -- |
    -- | For an undirected graph this is equivalent to 'adjacentVertices', but
    -- | for the case of a directed graph, the directed arcs will constrain the
    -- | reachability of the adjacent vertices.
    reachableAdjacentVertices :: (Hashable v, Eq v) => g v e -> v -> [v]
    reachableAdjacentVertices g v = (\(_, v', _) -> v') <$> reachableAdjacentVertices' g v

    -- | Same as 'reachableAdjacentVertices' but gives back the connecting edges
    reachableAdjacentVertices' :: (Hashable v, Eq v) => g v e -> v -> [(v, v, e)]

    -- | Total number of incident edges of a vertex
    vertexDegree :: (Hashable v, Eq v) => g v e -> v -> Int

    -- | Degrees of a all the vertices in a graph
    degrees :: (Hashable v, Eq v) => g v e -> [Int]
    degrees g = vertexDegree g <$> vertices g

    -- | Maximum degree of a graph
    maxDegree :: (Hashable v, Eq v) => g v e -> Int
    maxDegree = maximum . degrees

    -- | Minimum degree of a graph
    minDegree :: (Hashable v, Eq v) => g v e -> Int
    minDegree = minimum . degrees

    -- | Average degree of a graph
    avgDegree :: (Hashable v, Eq v) => g v e -> Double
    avgDegree g = fromIntegral (2 * size g) / fromIntegral (order g)

    -- | Density of a graph
    -- | The ratio of the number of existing edges in the graph to the number of
    -- | posible edges
    density :: (Hashable v, Eq v) => g v e -> Double
    density g = (2 * (e - n + 1)) / (n * (n - 3) + 2)
        where
            n = fromIntegral $ order g
            e = fromIntegral $ size g

    -- | Insert a vertex into a graph
    -- | If the graph already contains the vertex leave the graph untouched
    insertVertex :: (Hashable v, Eq v) => v -> g v e -> g v e

    -- | Insert a many vertices into a graph
    -- | New vertices are inserted and already contained vertices are left
    -- | untouched
    insertVertices :: (Hashable v, Eq v) => [v] -> g v e -> g v e
    insertVertices vs g = foldl' (flip insertVertex) g vs

    -- | Tell if an edge exists in the graph
    containsEdgePair :: (Hashable v, Eq v) => g v e -> (v, v) -> Bool

    -- | Retrieve the incident edges of a vertex
    incidentEdgeTriples :: (Hashable v, Eq v) => g v e -> v -> [(v, v, e)]

    -- | Retrieve the incident edges of a vertex, ignoring its attributes
    incidentEdgePairs :: (Hashable v, Eq v) => g v e -> v -> [(v, v)]
    incidentEdgePairs g v = tripleToPair <$> incidentEdgeTriples g v

    -- | Insert an edge into a graph
    -- | The involved vertices are inserted if don't exist. If the graph already
    -- | contains the edge, its attribute is updated
    insertEdgeTriple :: (Hashable v, Eq v) => (v, v, e) -> g v e -> g v e

    -- | Same as 'insertEdgeTriple' but for multiple edges
    insertEdgeTriples :: (Hashable v, Eq v) => [(v, v, e)] -> g v e -> g v e
    insertEdgeTriples es g = foldl' (flip insertEdgeTriple) g es

    -- | Same as 'insertEdgeTriple' but insert edge pairs in graphs with
    -- | attributeless edges
    insertEdgePair :: (Hashable v, Eq v) => (v, v) -> g v () -> g v ()
    insertEdgePair (v1, v2) = insertEdgeTriple (v1, v2, ())

    -- | Same as 'insertEdgePair' for multiple edges
    insertEdgePairs :: (Hashable v, Eq v) => [(v, v)] -> g v () -> g v ()
    insertEdgePairs es g = foldl' (flip insertEdgePair) g es

    -- | Remove a vertex from a graph if present
    -- | Every edge incident to this vertex is also removed
    removeVertex :: (Hashable v, Eq v) => v -> g v e -> g v e

    -- | Same as 'removeVertex' but for multiple vertices
    removeVertices :: (Hashable v, Eq v) => [v] -> g v e -> g v e
    removeVertices vs g = foldl' (flip removeVertex) g vs

    -- | Remove an edge from a graph if present
    -- | The involved vertices are left untouched
    removeEdgePair :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e

    -- | Same as 'removeEdgePair' but for multple edges
    removeEdgePairs :: (Hashable v, Eq v) => [(v, v)] -> g v e -> g v e
    removeEdgePairs es g = foldl' (flip removeEdgePair) g es

    -- | Remove the edge from a graph if present
    -- | The involved vertices are also removed
    removeEdgePairAndVertices :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e
    removeEdgePairAndVertices (v1, v2) g =
        removeVertex v2 $ removeVertex v1 $ removeEdgePair (v1, v2) g

    -- | Tell if a graph is simple
    -- | A graph is @simple@ if it has no loops
    isSimple :: (Hashable v, Eq v) => g v e -> Bool

    -- | Generate a graph of Int vertices from an adjacency
    -- | square matrix
    fromAdjacencyMatrix :: [[Int]] -> Maybe (g Int ())

    -- | Get the adjacency matrix representation of a grah
    toAdjacencyMatrix :: g v e -> [[Int]]

-- | Undirected Edge with attribute of type /e/ between to Vertices of type /v/
data Edge v e = Edge v v e
    deriving (Show, Read, Ord, Generic)

-- | Directed Arc with attribute of type /e/ between to Vertices of type /v/
data Arc v e = Arc v v e
    deriving (Show, Read, Ord, Generic)

-- | Construct an attributeless undirected 'Edge' between two vertices
(<->) :: (Hashable v) => v -> v -> Edge v ()
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct an attributeless directed 'Arc' between two vertices
(-->) :: (Hashable v) => v -> v -> Arc v ()
(-->) v1 v2 = Arc v1 v2 ()

class IsEdge e where
    -- | Convert an edge to a pair discarding its attribute
    toPair :: e v a -> (v, v)

    -- | Convert a pair to an edge, where it's attribute is unit
    fromPair :: (v, v) -> e v ()

    -- | Convert an edge to a triple, where the 3rd element it's the edge
    -- | attribute
    toTriple :: e v a -> (v, v, a)

    -- | Convert a triple to an edge
    fromTriple :: (v, v, a) -> e v a

    -- | Tell if an edge is a loop
    -- | An edge forms a @loop@ if both of its ends point to the same vertex
    isLoop :: (Eq v) => e v a -> Bool

instance (NFData v, NFData e) => NFData (Edge v e)
instance (NFData v, NFData e) => NFData (Arc v e)

instance IsEdge Edge where
    toPair (Edge v1 v2 _) = (v1, v2)
    fromPair (v1, v2) = Edge v1 v2 ()
    toTriple (Edge v1 v2 e) = (v1, v2, e)
    fromTriple (v1, v2, e) = Edge v1 v2 e
    isLoop (Edge v1 v2 _) = v1 == v2

instance IsEdge Arc where
    toPair (Arc fromV toV _) = (fromV, toV)
    fromPair (fromV, toV) = Arc fromV toV ()
    toTriple (Arc fromV toV e) = (fromV, toV, e)
    fromTriple (fromV, toV, e) = Arc fromV toV e
    isLoop (Arc v1 v2 _) = v1 == v2

-- | Weighted Edge attributes
-- | Useful for computing some algorithms on graphs
class Weighted a where
    weight :: a -> Double

-- | Labeled Edge attributes
-- | Useful for graph plotting
class Labeled a where
    label :: a -> String

instance Weighted Int where
    weight = fromIntegral

instance Weighted Float where
    weight = float2Double

instance Weighted Double where
    weight = id

instance Labeled String where
    label = id

instance Weighted (Double, String) where
    weight = fst

instance Labeled (Double, String) where
    label = snd

instance (Arbitrary v, Arbitrary e, Num v, Ord v) => Arbitrary (Edge v e) where
    arbitrary = arbitraryEdge Edge

instance (Arbitrary v, Arbitrary e, Num v, Ord v) => Arbitrary (Arc v e) where
    arbitrary = arbitraryEdge Arc

-- | To 'Edge's are equal if they point to the same vertices, regardless of the
-- | direction
instance (Eq v, Eq a) => Eq (Edge v a) where
    (Edge v1 v2 a) == (Edge v1' v2' a') =
        (a == a')
        && (v1 == v1' && v2 == v2')
        || (v1 == v2' && v2 == v1')

-- | To 'Arc's are equal if they point to the same vertices, and the directions
-- | is the same
instance (Eq v, Eq a) => Eq (Arc v a) where
    (Arc v1 v2 a) == (Arc v1' v2' a') = (a == a') && (v1 == v1' && v2 == v2')

-- | Convert a triple to a pair by ignoring the third element
tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (a, b, _) = (a, b)

-- | Edges generator
arbitraryEdge :: (Arbitrary v, Arbitrary e, Ord v, Num v)
 => (v -> v -> e -> edge) -> Gen edge
arbitraryEdge edgeType = edgeType <$> vert <*> vert <*> arbitrary
    where vert = getPositive <$> arbitrary






-- ###########
-- ## Internal
-- ###########

-- | Each vertex maps to a 'Links' value so it can poit to other vertices
type Links v e = HM.HashMap v e

-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink = HM.insert

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> HM.HashMap v (Links v e) -> Links v e
getLinks = HM.lookupDefault HM.empty

-- | Get 'Arc's from an association list of vertices and their links
linksToArcs :: [(v, Links v a)] -> [Arc v a]
linksToArcs = concatMap toArc
    where
        toArc :: (v, Links v a) -> [Arc v a]
        toArc (fromV, links) = fmap (uncurry (Arc fromV)) (HM.toList links)

-- | Get 'Edge's from an association list of vertices and their links
linksToEdges :: [(v, Links v a)] -> [Edge v a]
linksToEdges = concatMap toEdge
    where
        toEdge :: (v, Links v a) -> [Edge v a]
        toEdge (fromV, links) = fmap (uncurry (Edge fromV)) (HM.toList links)

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
