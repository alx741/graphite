{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.UGraph where

import Data.List (foldl')

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
newtype UGraph v e = UGraph { unUGraph :: HM.HashMap v (Links v e) }
    deriving (Eq, Show)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (UGraph v e) where
    arbitrary = insertEdges <$> pure empty <*> arbitrary

instance Graph UGraph where
    empty = UGraph HM.empty
    order (UGraph g) = HM.size g
    vertices (UGraph g) = HM.keys g
    edgePairs g = toPair <$> edges g

    containsVertex (UGraph g) = flip HM.member g
    areAdjacent (UGraph g) v1 v2 = HM.member v2 $ getLinks v1 g
    adjacentVertices (UGraph g) v = HM.keys $ getLinks v g
    directlyReachableVertices g v = v : (adjacentVertices g v)
    vertexDegree (UGraph g) v = length $ HM.keys $ getLinks v g
    insertVertex (UGraph g) v = UGraph $ hashMapInsert v HM.empty g
    insertVertices = foldl' insertVertex

    containsEdgePair = containsEdge'
    incidentEdgePairs g v = fmap toPair $ incidentEdges g v
    insertEdgePair g (v1, v2) = insertEdge g (Edge v1 v2 ())
    removeEdgePair = removeEdge'
    removeEdgePairAndVertices = removeEdgeAndVertices'

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && (not $ HM.member v $ getLinks v $ unUGraph g)

    fromAdjacencyMatrix m
        | length m /= length (head m) = Nothing
        | otherwise = Just $ insertEdges empty (foldl' genEdges [] labeledM)
            where
                labeledM :: [(Int, [(Int, Int)])]
                labeledM = zip [1..] $ fmap (zip [1..]) m

                genEdges :: [Edge Int ()] -> (Int, [(Int, Int)]) -> [Edge Int ()]
                genEdges es (i, vs) = es ++ fmap (\v -> Edge i v ()) connected
                    where connected = fst <$> filter (\(_, v) -> v /= 0) vs

    toAdjacencyMatrix = undefined



-- | @O(n)@ Remove a vertex from a 'UGraph' if present
-- | Every 'Edge' incident to this vertex is also removed
removeVertex :: (Hashable v, Eq v) => v -> UGraph v e -> UGraph v e
removeVertex v g = UGraph
    $ (\(UGraph g') -> HM.delete v g')
    $ foldl' removeEdge g $ incidentEdges g v

-- | @O(log n)@ Insert an undirected 'Edge' into a 'UGraph'
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Edge, its attribute is updated
insertEdge :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> UGraph v e
insertEdge g (Edge v1 v2 edgeAttr) = UGraph $ link v2 v1 $ link v1 v2 g'
    where
        g' = unUGraph $ insertVertices g [v1, v2]
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | @O(m*log n)@ Insert many directed 'Edge's into a 'UGraph'
-- | Same rules as 'insertEdge' are applied
insertEdges :: (Hashable v, Eq v) => UGraph v e -> [Edge v e] -> UGraph v e
insertEdges = foldl' insertEdge

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph' if present
-- | The involved vertices are left untouched
removeEdge :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> UGraph v e
removeEdge g = removeEdgePair g . toPair

-- | Same as 'removeEdge' but the edge is an unordered pair
removeEdge' :: (Hashable v, Eq v) => UGraph v e -> (v, v) -> UGraph v e
removeEdge' graph@(UGraph g) (v1, v2)
    | containsVertex graph v1 && containsVertex graph v2 =
        UGraph $ update v2Links v2 $ update v1Links v1 g
    | otherwise = UGraph g
    where
        v1Links = HM.delete v2 $ getLinks v1 g
        v2Links = HM.delete v1 $ getLinks v2 g
        update = HM.adjust . const

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph' if present
-- | The involved vertices are also removed
removeEdgeAndVertices :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> UGraph v e
removeEdgeAndVertices g = removeEdgePairAndVertices g . toPair

-- | Same as 'removeEdgeAndVertices' but the edge is an unordered pair
removeEdgeAndVertices' :: (Hashable v, Eq v) => UGraph v e -> (v, v) -> UGraph v e
removeEdgeAndVertices' g (v1, v2) =
    removeVertex v2 $ removeVertex v1 $ removeEdgePair g (v1, v2)

-- | @O(n*m)@ Retrieve the 'Edge's of a 'UGraph'
edges :: forall v e . (Hashable v, Eq v) => UGraph v e -> [Edge v e]
edges (UGraph g) = linksToEdges $ zip vs links
    where
        vs :: [v]
        vs = vertices $ UGraph g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | @O(log n)@ Tell if an undirected 'Edge' exists in the graph
containsEdge :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> Bool
containsEdge g = containsEdge' g . toPair

-- | Same as 'containsEdge' but the edge is an unordered pair
containsEdge' :: (Hashable v, Eq v) => UGraph v e -> (v, v) -> Bool
containsEdge' graph@(UGraph g) (v1, v2) =
    containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
    where v1Links = getLinks v1 g

-- | Retrieve the incident 'Edge's of a Vertex
incidentEdges :: (Hashable v, Eq v) => UGraph v e -> v -> [Edge v e]
incidentEdges (UGraph g) v = fmap (uncurry (Edge v)) (HM.toList (getLinks v g))
