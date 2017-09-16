{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.UGraph where

import Data.List     (foldl', reverse, sort)

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
newtype UGraph v e = UGraph { unUGraph :: HM.HashMap v (Links v e) }
    deriving (Eq, Show)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (UGraph v e) where
    arbitrary = insertEdges <$> arbitrary <*> pure empty

instance Graph UGraph where
    empty = UGraph HM.empty
    order (UGraph g) = HM.size g
    vertices (UGraph g) = HM.keys g
    edgePairs g = toPair <$> edges g

    containsVertex (UGraph g) = flip HM.member g
    adjacentVertices (UGraph g) v = HM.keys $ getLinks v g
    vertexDegree (UGraph g) v = length $ HM.keys $ getLinks v g
    insertVertex v (UGraph g) = UGraph $ hashMapInsert v HM.empty g
    insertVertices vs g = foldl' (flip insertVertex) g vs

    containsEdgePair = containsEdge'
    incidentEdgePairs g v = fmap toPair $ incidentEdges g v
    insertEdgePair (v1, v2) g = insertEdge (Edge v1 v2 ()) g
    removeEdgePair = removeEdge'
    removeEdgePairAndVertices = removeEdgeAndVertices'

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && (not $ HM.member v $ getLinks v $ unUGraph g)

    isRegular = undefined

    fromAdjacencyMatrix m
        | length m /= length (head m) = Nothing
        | otherwise = Just $ insertEdges (foldl' genEdges [] labeledM) empty
            where
                labeledM :: [(Int, [(Int, Int)])]
                labeledM = zip [1..] $ fmap (zip [1..]) m

                genEdges :: [Edge Int ()] -> (Int, [(Int, Int)]) -> [Edge Int ()]
                genEdges es (i, vs) = es ++ fmap (\v -> Edge i v ()) connected
                    where connected = fst <$> filter (\(_, v) -> v /= 0) vs

    toAdjacencyMatrix = undefined



-- | @O(n)@ Remove a vertex from a 'UGraph if present
-- | Every 'Edge' incident to this vertex is also removed
removeVertex :: (Hashable v, Eq v) => v -> UGraph v e -> UGraph v e
removeVertex v g = UGraph
    $ (\(UGraph g') -> HM.delete v g')
    $ foldl' (flip removeEdge) g $ incidentEdges g v

-- | @O(log n)@ Insert an undirected 'Edge' into a 'UGraph
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Edge, its attribute is updated
insertEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
insertEdge (Edge v1 v2 edgeAttr) g = UGraph $ link v2 v1 $ link v1 v2 g'
    where
        g' = unUGraph $ insertVertices [v1, v2] g
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | @O(m*log n)@ Insert many directed 'Edge's into a 'UGraph
-- | Same rules as 'insertEdge' are applied
insertEdges :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e -> UGraph v e
insertEdges es g = foldl' (flip insertEdge) g es

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph if present
-- | The involved vertices are left untouched
removeEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdge = removeEdgePair . toPair

-- | Same as 'removeEdge' but the edge is an unordered pair
removeEdge' :: (Hashable v, Eq v) => (v, v) -> UGraph v e -> UGraph v e
removeEdge' (v1, v2) graph@(UGraph g)
    | containsVertex graph v1 && containsVertex graph v2 =
        UGraph $ update v2Links v2 $ update v1Links v1 g
    | otherwise = UGraph g
    where
        v1Links = HM.delete v2 $ getLinks v1 g
        v2Links = HM.delete v1 $ getLinks v2 g
        update = HM.adjust . const

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph if present
-- | The involved vertices are also removed
removeEdgeAndVertices :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdgeAndVertices = removeEdgePairAndVertices . toPair

-- | Same as 'removeEdgeAndVertices' but the edge is an unordered pair
removeEdgeAndVertices' :: (Hashable v, Eq v) => (v, v) -> UGraph v e -> UGraph v e
removeEdgeAndVertices' (v1, v2) g =
    removeVertex v2 $ removeVertex v1 $ removeEdgePair (v1, v2) g

-- | @O(n*m)@ Retrieve the 'Edge's of a 'UGraph
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

-- | Tell if two 'UGraph are isomorphic
areIsomorphic :: UGraph v e -> UGraph v' e' -> Bool
areIsomorphic = undefined

isomorphism :: UGraph v e -> UGraph v' e' -> (v -> v')
isomorphism = undefined


-- | The Degree Sequence of a simple 'UGraph is a list of degrees
newtype DegreeSequence = DegreeSequence { unDegreeSequence :: [Int]}
    deriving (Eq, Ord, Show)

-- | Construct a 'DegreeSequence' from a list of degrees
-- | Negative degree values are discarded
degreeSequence :: [Int] -> DegreeSequence
degreeSequence = DegreeSequence . reverse . sort . filter (>0)

-- | Get the 'DegreeSequence' of a simple 'UGraph
-- | If the graph is not @simple@ (see 'isSimple') the result is Nothing
getDegreeSequence :: (Hashable v, Eq v) => UGraph v e -> Maybe DegreeSequence
getDegreeSequence g
    | (not . isSimple) g = Nothing
    | otherwise = Just $ degreeSequence $ degrees g

-- | Tell if a 'DegreeSequence' is a Graphical Sequence
-- | A Degree Sequence is a @Graphical Sequence@ if a corresponding 'UGraph for
-- | it exists
isGraphicalSequence :: DegreeSequence -> Bool
isGraphicalSequence = even . length . filter odd . unDegreeSequence

-- | Get the corresponding 'UGraph of a 'DegreeSequence'
-- | If the 'DegreeSequence' is not graphical (see 'isGraphicalSequence') the
-- | result is Nothing
fromGraphicalSequence :: DegreeSequence -> Maybe (UGraph Int ())
fromGraphicalSequence = undefined
