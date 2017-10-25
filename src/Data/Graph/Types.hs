{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Types
    (
    -- * Main Graph type class
    Graph(..)

    -- * Edges type class
    , IsEdge(..)
    -- ** Main IsEdge instances
    , Edge(..)
    , Arc(..)
    -- ** Edges and Arcs constructors
    , (<->)
    , (-->)
    -- ** Edge attributes type clases
    , Weighted(..)
    , Labeled(..)
    -- ** Triple-Edges convenience functions
    , tripleToPair
    , pairToTriple
    , tripleOriginVertex
    , tripleDestVertex
    , tripleAttribute
    ) where

import Data.List    (foldl')
import GHC.Float    (float2Double)
import GHC.Generics (Generic)

import Control.DeepSeq
import Data.Hashable
import Test.QuickCheck

-- | Types that behave like graphs
--
-- The main 'Graph' instances are 'UGraph' and 'DGraph'. The functions in this
-- class should be used for algorithms that are graph-directionality agnostic,
-- otherwise use the more specific ones in 'UGraph' and 'DGraph'
class Graph g where
    -- | The Empty (order-zero) graph with no vertices and no edges
    empty :: (Hashable v) => g v e

    -- | Retrieve the order of a graph
    --
    -- The @order@ of a graph is its number of vertices
    order :: g v e -> Int

    -- | Retrieve the size of a graph
    --
    -- The @size@ of a graph is its number of edges
    size :: (Hashable v, Eq v) => g v e -> Int
    size = length . edgePairs

    -- | Density of a graph
    --
    -- The @density@ of a graph is the ratio of the number of existing edges to
    -- the number of posible edges
    density :: (Hashable v, Eq v) => g v e -> Double
    density g = (2 * (e - n + 1)) / (n * (n - 3) + 2)
        where
            n = fromIntegral $ order g
            e = fromIntegral $ size g


    -- * Operations

    -- | Retrieve all the vertices of a graph
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
    adjacentVertices g v = tripleDestVertex <$> adjacentVertices' g v

    -- | Same as 'adjacentVertices' but gives back the connecting edges
    adjacentVertices' :: (Hashable v, Eq v) => g v e -> v -> [(v, v, e)]

    -- | Same as 'adjacentVertices' but gives back only those vertices for which
    -- the connecting edge allows the vertex to be reached.
    --
    -- For an undirected graph this is equivalent to 'adjacentVertices', but
    -- for the case of a directed graph, the directed arcs will constrain the
    -- reachability of the adjacent vertices.
    reachableAdjacentVertices :: (Hashable v, Eq v) => g v e -> v -> [v]
    reachableAdjacentVertices g v = tripleDestVertex <$> reachableAdjacentVertices' g v

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

    -- | Insert a vertex into a graph. If the graph already contains the vertex
    -- leave it untouched
    insertVertex :: (Hashable v, Eq v) => v -> g v e -> g v e

    -- | Insert many vertices into a graph. New vertices are inserted and
    -- already contained vertices are left untouched
    insertVertices :: (Hashable v, Eq v) => [v] -> g v e -> g v e
    insertVertices vs g = foldl' (flip insertVertex) g vs

    -- | Tell if an edge exists in the graph
    containsEdgePair :: (Hashable v, Eq v) => g v e -> (v, v) -> Bool

    -- | Retrieve the incident edges of a vertex
    incidentEdgeTriples :: (Hashable v, Eq v) => g v e -> v -> [(v, v, e)]

    -- | Retrieve the incident edges of a vertex, ignoring its attributes
    incidentEdgePairs :: (Hashable v, Eq v) => g v e -> v -> [(v, v)]
    incidentEdgePairs g v = tripleToPair <$> incidentEdgeTriples g v

    -- | Get the edge between to vertices if it exists
    edgeTriple :: (Hashable v, Eq v) => g v e -> v -> v -> Maybe (v, v, e)

    -- | Insert an edge into a graph. The involved vertices are inserted if
    -- don't exist. If the graph already contains the edge, its attribute gets
    -- updated
    insertEdgeTriple :: (Hashable v, Eq v) => (v, v, e) -> g v e -> g v e

    -- | Same as 'insertEdgeTriple' but for multiple edges
    insertEdgeTriples :: (Hashable v, Eq v) => [(v, v, e)] -> g v e -> g v e
    insertEdgeTriples es g = foldl' (flip insertEdgeTriple) g es

    -- | Same as 'insertEdgeTriple' but insert edge pairs in graphs with
    -- attribute less edges
    insertEdgePair :: (Hashable v, Eq v) => (v, v) -> g v () -> g v ()
    insertEdgePair (v1, v2) = insertEdgeTriple (v1, v2, ())

    -- | Same as 'insertEdgePair' for multiple edges
    insertEdgePairs :: (Hashable v, Eq v) => [(v, v)] -> g v () -> g v ()
    insertEdgePairs es g = foldl' (flip insertEdgePair) g es

    -- | Remove a vertex from a graph if present. Every edge incident to this
    -- vertex also gets removed
    removeVertex :: (Hashable v, Eq v) => v -> g v e -> g v e

    -- | Same as 'removeVertex' but for multiple vertices
    removeVertices :: (Hashable v, Eq v) => [v] -> g v e -> g v e
    removeVertices vs g = foldl' (flip removeVertex) g vs

    -- | Remove an edge from a graph if present. The involved vertices are left
    -- untouched
    removeEdgePair :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e

    -- | Same as 'removeEdgePair' but for multiple edges
    removeEdgePairs :: (Hashable v, Eq v) => [(v, v)] -> g v e -> g v e
    removeEdgePairs es g = foldl' (flip removeEdgePair) g es

    -- | Remove the edge from a graph if present. The involved vertices also get
    -- removed
    removeEdgePairAndVertices :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e
    removeEdgePairAndVertices (v1, v2) g =
        removeVertex v2 $ removeVertex v1 $ removeEdgePair (v1, v2) g

    -- | Retrieve the isolated vertices of a graph, if any
    isolatedVertices :: (Hashable v, Eq v) => g v e -> [v]
    isolatedVertices g = filter (\v -> vertexDegree g v == 0) $ vertices g

    -- | Tell if a graph is simple
    --
    -- A graph is @simple@ if it has no loops
    isSimple :: (Hashable v, Eq v) => g v e -> Bool


    -- * Binary operations

    -- | Union of two graphs
    union :: (Hashable v, Eq v) => g v e -> g v e -> g v e

    -- | Intersection of two graphs
    intersection :: (Hashable v, Eq v, Eq e) => g v e -> g v e -> g v e


    -- * Transformations

    -- | Convert a graph to an adjacency list with vertices in type /v/ and edge
    -- attributes in /e/
    toList :: (Hashable v, Eq v) => g v e -> [(v, [(v, e)])]

    -- | Construct a graph from an adjacency list with vertices in type /v and
    -- edge attributes in /e/
    fromList :: (Hashable v, Eq v) => [(v, [(v, e)])] -> g v e
    fromList links = go links empty
        where
            go [] g = g
            go ((v, es):rest) g = go
                rest $
                foldr
                    (\(v', e) g' -> insertEdgeTriple (v, v', e) g')
                    (insertVertex v g)
                    es

    -- TODO: make this [[Bool]]
    -- | Get the adjacency binary matrix representation of a graph
    toAdjacencyMatrix :: g v e -> [[Int]]

    -- | Generate a graph of Int vertices from an adjacency square binary matrix
    fromAdjacencyMatrix :: [[Int]] -> Maybe (g Int ())



-- | Types that represent edges
--
-- The main 'IsEdge' instances are 'Edge' for undirected edges and 'Arc' for
-- directed edges.
class IsEdge e where
    -- | Retrieve the origin vertex of the edge
    originVertex :: e v a -> v

    -- | Retrieve the destination vertex of the edge
    destinationVertex :: e v a -> v

    -- | Retrieve the attribute of the edge
    attribute :: e v a -> a

    -- * Conversion

    -- | Convert an edge to a pair discarding its attribute
    toPair :: e v a -> (v, v)

    -- | Convert a pair to an edge, where it's attribute is unit
    fromPair :: (v, v) -> e v ()

    -- | Convert an edge to a triple, where the 3rd element it's the edge
    -- attribute
    toTriple :: e v a -> (v, v, a)

    -- | Convert a triple to an edge
    fromTriple :: (v, v, a) -> e v a


    -- * Properties

    -- | Tell if an edge is a loop
    --
    -- An edge forms a @loop@ if both of its ends point to the same vertex
    isLoop :: (Eq v) => e v a -> Bool



-- | Undirected Edge with attribute of type /e/ between to Vertices of type /v/
data Edge v e = Edge v v e
    deriving (Show, Read, Ord, Generic)

-- | Directed Arc with attribute of type /e/ between to Vertices of type /v/
data Arc v e = Arc v v e
    deriving (Show, Read, Ord, Generic)

-- | Construct an attribute less undirected 'Edge' between two vertices
(<->) :: (Hashable v) => v -> v -> Edge v ()
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct an attribute less directed 'Arc' between two vertices
(-->) :: (Hashable v) => v -> v -> Arc v ()
(-->) v1 v2 = Arc v1 v2 ()

instance (NFData v, NFData e) => NFData (Edge v e)
instance (NFData v, NFData e) => NFData (Arc v e)

instance IsEdge Edge where
    originVertex (Edge v _ _) = v
    destinationVertex (Edge _ v _) = v
    attribute (Edge _ _ e) = e
    toPair (Edge v1 v2 _) = (v1, v2)
    fromPair (v1, v2) = Edge v1 v2 ()
    toTriple (Edge v1 v2 e) = (v1, v2, e)
    fromTriple (v1, v2, e) = Edge v1 v2 e
    isLoop (Edge v1 v2 _) = v1 == v2

instance IsEdge Arc where
    originVertex (Arc v _ _) = v
    destinationVertex (Arc _ v _) = v
    attribute (Arc _ _ e) = e
    toPair (Arc fromV toV _) = (fromV, toV)
    fromPair (fromV, toV) = Arc fromV toV ()
    toTriple (Arc fromV toV e) = (fromV, toV, e)
    fromTriple (fromV, toV, e) = Arc fromV toV e
    isLoop (Arc v1 v2 _) = v1 == v2

-- | Weighted Edge attributes
class Weighted e where
    weight :: e -> Double

-- | Labeled Edge attributes
class Labeled e where
    label :: e -> String

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

instance Functor (Edge v) where
    fmap f (Edge v1 v2 e) = Edge v1 v2 $ f e

instance Functor (Arc v) where
    fmap f (Arc v1 v2 e) = Arc v1 v2 $ f e

-- | Edges generator
arbitraryEdge :: (Arbitrary v, Arbitrary e, Ord v, Num v)
 => (v -> v -> e -> edge) -> Gen edge
arbitraryEdge edgeType = edgeType <$> vert <*> vert <*> arbitrary
    where vert = getPositive <$> arbitrary

-- | Two 'Edge's are equal if they point to the same vertices, regardless of the
-- direction
instance (Eq v, Eq a) => Eq (Edge v a) where
    (Edge v1 v2 a) == (Edge v1' v2' a') =
        (a == a')
        && (v1 == v1' && v2 == v2')
        || (v1 == v2' && v2 == v1')

-- | Two 'Arc's are equal if they point to the same vertices, and the directions
-- are the same
instance (Eq v, Eq a) => Eq (Arc v a) where
    (Arc v1 v2 a) == (Arc v1' v2' a') = (a == a') && (v1 == v1' && v2 == v2')


-- | Convert a triple to a pair by ignoring the third element
tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (a, b, _) = (a, b)

-- | Convert a pair to a triple where the 3rd element is unit
pairToTriple :: (a, b) -> (a, b, ())
pairToTriple (a, b) = (a, b, ())

-- | Get the origin vertex from an edge triple
tripleOriginVertex :: (v, v, e) -> v
tripleOriginVertex (v, _, _) = v

-- | Get the destination vertex from an edge triple
tripleDestVertex :: (v, v, e) -> v
tripleDestVertex (_, v, _) = v

-- | Get the attribute from an edge triple
tripleAttribute :: (v, v, e) -> e
tripleAttribute (_, _, e) = e
