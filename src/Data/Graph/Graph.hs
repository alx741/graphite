{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Graph where

import Control.Monad (replicateM)
import Data.List     (foldl', reverse, sort)
import System.Random

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
newtype Graph v e = Graph { unGraph :: HM.HashMap v (Links v e) }
    deriving (Eq, Show)

instance IsGraph Graph where
    empty = Graph HM.empty
    order (Graph g) = HM.size g
    size = length . edges
    vertices (Graph g) = HM.keys g
    edgePairs g = toUnorderedPair <$> edges g

    containsVertex (Graph g) = flip HM.member g
    adjacentVertices (Graph g) v = HM.keys $ getLinks v g
    vertexDegree (Graph g) v = length $ HM.keys $ getLinks v g
    insertVertex v (Graph g) = Graph $ hashMapInsert v HM.empty g
    insertVertices vs g = foldl' (flip insertVertex) g vs

    containsEdgePair = containsEdge'
    incidentEdgePairs g v = fmap toUnorderedPair $ incidentEdges g v
    insertEdgePair (v1, v2) g = insertEdge (Edge v1 v2 ()) g
    removeEdgePair = removeEdge'
    removeEdgePairAndVertices = removeEdgeAndVertices'

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && (not $ HM.member v $ getLinks v $ unGraph g)

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



instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (Graph v e) where
    arbitrary = insertEdges <$> arbitrary <*> pure empty

-- | Probability value between 0 and 1
newtype Probability = P Float deriving (Eq, Ord, Show)

-- | Construct a 'Probability' value
probability :: Float -> Probability
probability v | v >= 1 = P 1 | v <= 0 = P 0 | otherwise = P v

-- | Generate a random 'Graph' of the Erdős–Rényi G(n, p) model
erdosRenyiIO :: Int -> Probability -> IO (Graph Int ())
erdosRenyiIO n (P p) = go [1..n] p empty
    where
        go :: [Int] -> Float -> Graph Int () -> IO (Graph Int ())
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- randomRs (0.0, 1.0) <$> newStdGen
            let vs' = zip rnds vs
            go vs pv $! (foldl' (putV pv v) g vs')

        putV :: Float -> Int -> Graph Int () -> (Float, Int) -> Graph Int ()
        putV pv v g (p', v') | p' < pv = insertEdge (v <-> v') g | otherwise = g


randomMatIO :: Int -> IO [[Int]]
randomMatIO n = replicateM n randRow
    where randRow = replicateM n (randomRIO (0,1)) :: IO [Int]

-- | @O(n)@ Remove a vertex from a 'Graph' if present
-- | Every 'Edge' incident to this vertex is also removed
removeVertex :: (Hashable v, Eq v) => v -> Graph v e -> Graph v e
removeVertex v g = Graph
    $ (\(Graph g') -> HM.delete v g')
    $ foldl' (flip removeEdge) g $ incidentEdges g v

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
insertEdges es g = foldl' (flip insertEdge) g es

-- | @O(log n)@ Remove the undirected 'Edge' from a 'Graph' if present
-- | The involved vertices are left untouched
removeEdge :: (Hashable v, Eq v) => Edge v e -> Graph v e -> Graph v e
removeEdge = removeEdgePair . toUnorderedPair

-- | Same as 'removeEdge' but the edge is an unordered pair
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
removeEdgeAndVertices = removeEdgePairAndVertices . toUnorderedPair

-- | Same as 'removeEdgeAndVertices' but the edge is an unordered pair
removeEdgeAndVertices' :: (Hashable v, Eq v) => (v, v) -> Graph v e -> Graph v e
removeEdgeAndVertices' (v1, v2) g =
    removeVertex v2 $ removeVertex v1 $ removeEdgePair (v1, v2) g

-- | @O(n*m)@ Retrieve the 'Edge's of a 'Graph'
edges :: forall v e . (Hashable v, Eq v) => Graph v e -> [Edge v e]
edges (Graph g) = linksToEdges $ zip vs links
    where
        vs :: [v]
        vs = vertices $ Graph g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | @O(log n)@ Tell if an undirected 'Edge' exists in the graph
containsEdge :: (Hashable v, Eq v) => Graph v e -> Edge v e -> Bool
containsEdge g = containsEdge' g . toUnorderedPair

-- | Same as 'containsEdge' but the edge is an unordered pair
containsEdge' :: (Hashable v, Eq v) => Graph v e -> (v, v) -> Bool
containsEdge' graph@(Graph g) (v1, v2) =
    containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
    where v1Links = getLinks v1 g

-- | Retrieve the incident 'Edge's of a Vertex
incidentEdges :: (Hashable v, Eq v) => Graph v e -> v -> [Edge v e]
incidentEdges (Graph g) v = fmap (uncurry (Edge v)) (HM.toList (getLinks v g))

-- | Tell if an 'Edge' forms a loop
-- | An 'Edge' forms a loop with both of its ends point to the same vertex
isLoop :: (Eq v) => Edge v e -> Bool
isLoop (Edge v1 v2 _) = v1 == v2

-- | Tell if two 'Graph's are isomorphic
areIsomorphic :: Graph v e -> Graph v' e' -> Bool
areIsomorphic = undefined

isomorphism :: Graph v e -> Graph v' e' -> (v -> v')
isomorphism = undefined


-- | The Degree Sequence of a simple 'Graph' is a list of degrees
newtype DegreeSequence = DegreeSequence { unDegreeSequence :: [Int]}
    deriving (Eq, Ord, Show)

-- | Construct a 'DegreeSequence' from a list of degrees
-- | Negative degree values are discarded
degreeSequence :: [Int] -> DegreeSequence
degreeSequence = DegreeSequence . reverse . sort . filter (>0)

-- | Get the 'DegreeSequence' of a simple 'Graph'
-- | If the graph is not @simple@ (see 'isSimple') the result is Nothing
getDegreeSequence :: (Hashable v, Eq v) => Graph v e -> Maybe DegreeSequence
getDegreeSequence g
    | (not . isSimple) g = Nothing
    | otherwise = Just $ degreeSequence $ degrees g

-- | Tell if a 'DegreeSequence' is a Graphical Sequence
-- | A Degree Sequence is a @Graphical Sequence@ if a corresponding 'Graph' for
-- | it exists
isGraphicalSequence :: DegreeSequence -> Bool
isGraphicalSequence = even . length . filter odd . unDegreeSequence

-- | Get the corresponding 'Graph' of a 'DegreeSequence'
-- | If the 'DegreeSequence' is not graphical (see 'isGraphicalSequence') the
-- | result is Nothing
fromGraphicalSequence :: DegreeSequence -> Maybe (Graph Int ())
fromGraphicalSequence = undefined
