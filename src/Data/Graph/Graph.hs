{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Graph where

import Data.List     (foldl')
-- import System.Random

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
newtype Graph v e = Graph { unGraph :: HM.HashMap v (Links v e) }
    deriving (Eq, Show)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (Graph v e) where
    arbitrary = insertEdges <$> arbitrary <*> pure empty

-- randomGraph :: (Hashable v, Enum v, Eq v, RandomGen g)
--  => (v, v)
--  -> g
--  -> (Graph v (), g)
-- randomGraph (lo, hi) gen = (insertVertices vs empty, gen)
--     where vs = enumFromTo lo hi

-- randomGraphIO :: (Hashable v, Enum v, Eq v) => (v, v) -> IO (Graph v ())
-- randomGraphIO (lo, hi) = do
--     gen <- getStdGen
--     return $ fst $ randomGraph (lo, hi) gen

-- | The Empty (order-zero) 'Graph' with no vertices and no edges
empty :: (Hashable v) => Graph v e
empty = Graph HM.empty

-- | Generate a directed 'Graph' of Int vertices from an adjacency
-- | square matrix
fromAdjacencyMatrix :: [[Int]] -> Maybe (Graph Int ())
fromAdjacencyMatrix m
    | length m /= length (head m) = Nothing
    | otherwise = Just $ insertEdges (foldl genEdges [] labeledM) empty
        where
            labeledM :: [(Int, [(Int, Int)])]
            labeledM = zip [1..] $ fmap (zip [1..]) m

            genEdges :: [Edge Int ()] -> (Int, [(Int, Int)]) -> [Edge Int ()]
            genEdges es (i, vs) = es ++ fmap (\v -> Edge i v ()) connected
                where connected = fmap fst $ filter (\(_, v) -> v /= 0) vs

-- | @O(log n)@ Insert a vertex into a 'Graph'
-- | If the graph already contains the vertex leave the graph untouched
insertVertex :: (Hashable v, Eq v) => v -> Graph v e -> Graph v e
insertVertex v (Graph g) = Graph $ hashMapInsert v HM.empty g

-- | @O(n)@ Remove a vertex from a 'Graph' if present
-- | Every 'Edge' incident to this vertex is also removed
removeVertex :: (Hashable v, Eq v) => v -> Graph v e -> Graph v e
removeVertex v g = Graph
    $ (\(Graph g') -> HM.delete v g')
    $ foldl' (flip removeEdge) g $ incidentEdges g v

-- | @O(m*log n)@ Insert a many vertices into a 'Graph'
-- | New vertices are inserted and already contained vertices are left untouched
insertVertices :: (Hashable v, Eq v) => [v] -> Graph v e -> Graph v e
insertVertices vs g = foldl' (flip insertVertex) g vs

-- | @O(log n)@ Insert an undirected 'Edge' into a 'Graph'
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Edge, its attribute is updated
insertEdge :: (Hashable v, Eq v) => Edge v e -> Graph v e -> Graph v e
insertEdge (Edge v1 v2 edgeAttr) g = Graph $ link v2 v1 $ link v1 v2 g'
    where
        g' = unGraph $ insertVertices [v1, v2] g
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | @O(m*log n)@ Insert many directed 'Edge's into a 'Graph'
-- | Same rules as 'insertEdge' are applied
insertEdges :: (Hashable v, Eq v) => [Edge v e] -> Graph v e -> Graph v e
insertEdges as g = foldl' (flip insertEdge) g as

-- | @O(log n)@ Remove the undirected 'Edge' from a 'Graph' if present
-- | The involved vertices are left untouched
removeEdge :: (Hashable v, Eq v) => Edge v e -> Graph v e -> Graph v e
removeEdge = removeEdge' . toUnorderedPair

-- | Same as 'removeEdge' but the edge is an unordered tuple
removeEdge' :: (Hashable v, Eq v) => (v, v) -> Graph v e -> Graph v e
removeEdge' (v1, v2) graph@(Graph g)
    | containsVertex graph v1 && containsVertex graph v2 =
        Graph $ update v2Links v2 $ update v1Links v1 g
    | otherwise = Graph g
    where
        v1Links = HM.delete v2 $ getLinks v1 g
        v2Links = HM.delete v1 $ getLinks v2 g
        update = HM.adjust . const

-- | @O(log n)@ Remove the undirected 'Edge' from a 'Graph' if present
-- | The involved vertices are also removed
removeEdgeAndVertices :: (Hashable v, Eq v) => Edge v e -> Graph v e -> Graph v e
removeEdgeAndVertices = removeEdgeAndVertices' . toUnorderedPair

-- | Same as 'removeEdgeAndVertices' but the edge is an unordered tuple
removeEdgeAndVertices' :: (Hashable v, Eq v) => (v, v) -> Graph v e -> Graph v e
removeEdgeAndVertices' (v1, v2) g =
    removeVertex v2 $ removeVertex v1 $ removeEdge' (v1, v2) g

-- | @O(n)@ Retrieve the vertices of a 'Graph'
vertices :: Graph v e -> [v]
vertices (Graph g) = HM.keys g

-- | @O(n)@ Retrieve the order of a 'Graph'
-- | The @order@ of a graph is its number of vertices
order :: Graph v e -> Int
order (Graph g) = HM.size g

-- | @O(n*m)@ Retrieve the size of a 'Graph'
-- | The @size@ of an undirected graph is its number of 'Edge's
size :: (Hashable v, Eq v) => Graph v e -> Int
size = length . edges

-- | @O(n*m)@ Retrieve the 'Edge's of a 'Graph'
edges :: forall v e . (Hashable v, Eq v) => Graph v e -> [Edge v e]
edges (Graph g) = linksToEdges $ zip vs links
    where
        vs :: [v]
        vs = vertices $ Graph g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | Same as 'edges' but the edges are unordered tuples, and their attributes
-- | are discarded
edges' :: (Hashable v, Eq v) => Graph v e -> [(v, v)]
edges' g = toUnorderedPair <$> edges g

-- | @O(log n)@ Tell if a vertex exists in the graph
containsVertex :: (Hashable v, Eq v) => Graph v e -> v -> Bool
containsVertex (Graph g) = flip HM.member g

-- | @O(log n)@ Tell if an undirected 'Edge' exists in the graph
containsEdge :: (Hashable v, Eq v) => Graph v e -> Edge v e -> Bool
containsEdge g = containsEdge' g . toUnorderedPair

-- | Same as 'containsEdge' but the edge is an unordered tuple
containsEdge' :: (Hashable v, Eq v) => Graph v e -> (v, v) -> Bool
containsEdge' graph@(Graph g) (v1, v2) =
    containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
    where v1Links = getLinks v1 g

-- | Retrieve the incident 'Edge's of a Vertex
incidentEdges :: (Hashable v, Eq v) => Graph v e -> v -> [Edge v e]
incidentEdges g v = filter (\(Edge v1 v2 _) -> v == v1 || v == v2) $ edges g

-- | Degree of a vertex
-- | The total number incident 'Edge's of a vertex
vertexDegree :: (Hashable v, Eq v) => Graph v e -> v -> Int
vertexDegree g = length . incidentEdges g

-- | Degrees of a all the vertices in a 'Graph'
degrees :: (Hashable v, Eq v) => Graph v e -> [Int]
degrees g = fmap (vertexDegree g) $ vertices g

-- | Maximum degree of a 'Graph'
maxDegree :: (Hashable v, Eq v) => Graph v e -> Int
maxDegree = maximum . degrees

-- | Minimum degree of a 'Graph'
minDegree :: (Hashable v, Eq v) => Graph v e -> Int
minDegree = minimum . degrees

-- | Tell if an 'Edge' forms a loop
-- | An 'Edge' forms a loop with both of its ends point to the same vertex
isLoop :: (Eq v) => Edge v e -> Bool
isLoop (Edge v1 v2 _) = v1 == v2

-- | Tell if a 'Graph' is simple
-- | A 'Graph' is @simple@ if it has no multiple edges nor loops
isSimple :: (Hashable v, Eq v) => Graph v e -> Bool
isSimple = not . or . (map isLoop) . edges

-- | Tell if a 'Graph' is regular
-- | An Undirected Graph is @regular@ when all of its vertices have the same
-- | number of adjacent vertices
isRegular :: Graph v e -> Bool
isRegular = undefined
