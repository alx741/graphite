{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Types where

import Data.List (nubBy)
import GHC.Float (float2Double)

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

class IsGraph g where
    -- | The Empty (order-zero) graph with no vertices and no edges
    empty :: (Hashable v) => g v e

    -- | Retrieve the order of a graph
    -- | The @order@ of a graph is its number of vertices
    order :: g v e -> Int

    -- | Retrieve the size of a graph
    -- | The @size@ of a graph is its number of edges
    size :: (Hashable v, Eq v) => g v e -> Int

    -- | Retrieve the vertices of a graph
    vertices :: g v e -> [v]

    -- | Retrieve the edges of a graph as pairs
    edgePairs :: (Hashable v, Eq v) => g v e -> [(v, v)]

    -- | Tell if a vertex exists in the graph
    containsVertex :: (Hashable v, Eq v) => g v e -> v -> Bool

    -- | Retrieve the adjacent vertices of a vertex
    adjacentVertices :: (Hashable v, Eq v) => g v e -> v -> [v]

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

    -- | Insert a vertex into a graph
    -- | If the graph already contains the vertex leave the graph untouched
    insertVertex :: (Hashable v, Eq v) => v -> g v e -> g v e

    -- | Insert a many vertices into a graph
    -- | New vertices are inserted and already contained vertices are left
    -- | untouched
    insertVertices :: (Hashable v, Eq v) => [v] -> g v e -> g v e

    containsEdgePair :: (Hashable v, Eq v) => g v e -> (v, v) -> Bool
    incidentEdgePairs :: (Hashable v, Eq v) => g v e -> v -> [(v, v)]
    insertEdgePair :: (Hashable v, Eq v) => (v, v) -> g v () -> g v ()
    removeEdgePair :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e
    removeEdgePairAndVertices :: (Hashable v, Eq v) => (v, v) -> g v e -> g v e

    -- | Tell if a graph is simple
    -- | A graph is @simple@ if it has no multiple edges nor loops
    isSimple :: (Hashable v, Eq v) => g v e -> Bool

    -- | Tell if a graph is regular
    -- | A graph is @regular@ when all of its vertices have the same
    -- | number of adjacent vertices
    isRegular :: g v e -> Bool

    -- | Generate a graph of Int vertices from an adjacency
    -- | square matrix
    fromAdjacencyMatrix :: [[Int]] -> Maybe (g Int ())

    -- | Get the adjacency matrix representation of a grah
    toAdjacencyMatrix :: g v e -> [[Int]]

-- | Undirected Edge with attribute of type /e/ between to Vertices of type /v/
data Edge v e = Edge v v e
    deriving (Show, Read, Ord)

-- | Directed Arc with attribute of type /e/ between to Vertices of type /v/
data Arc v e = Arc v v e
    deriving (Show, Read, Ord)

-- | Construct an undirected 'Edge' between two vertices
(<->) :: (Hashable v) => v -> v -> Edge v ()
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct a directed 'Arc' between two vertices
(-->) :: (Hashable v) => v -> v -> Arc v ()
(-->) v1 v2 = Arc v1 v2 ()

class IsEdge e where
    -- | Convert an edge to a pair discargind its attribute
    toPair :: e v a -> (v, v)

    -- | Tell if an edge is a loop
    -- | An edge forms a @loop@ if both of its ends point to the same vertex
    isLoop :: (Eq v) => e v a -> Bool

instance IsEdge Edge where
    toPair (Edge v1 v2 _) = (v1, v2)
    isLoop (Edge v1 v2 _) = v1 == v2

instance IsEdge Arc where
    toPair (Arc fromV toV _) = (fromV, toV)
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

-- | Edges generator
arbitraryEdge :: (Arbitrary v, Arbitrary e, Ord v, Num v)
 => (v -> v -> e -> edge)
 -> Gen edge
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
linksToArcs ls = concat $ fmap toArc ls
    where
        toArc :: (v, Links v a) -> [Arc v a]
        toArc (fromV, links) = fmap (\(v, a) -> Arc fromV v a) (HM.toList links)

-- | Get 'Edge's from an association list of vertices and their links
linksToEdges :: (Eq v) => [(v, Links v a)] -> [Edge v a]
linksToEdges ls = nubBy shallowEdgeEq $ concat $ fmap toEdge ls
    where
        toEdge :: (v, Links v a) -> [Edge v a]
        toEdge (fromV, links) = fmap (\(v, a) -> Edge fromV v a) (HM.toList links)
        shallowEdgeEq (Edge v1 v2 _) (Edge v1' v2' _) =
               (v1 == v1' && v2 == v2')
            || (v1 == v2' && v2 == v1')

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
