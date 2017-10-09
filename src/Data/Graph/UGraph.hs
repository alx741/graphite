{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Graph.UGraph where

import qualified Data.Foldable as F (toList)
import           Data.List     (foldl')
import           GHC.Generics  (Generic)

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence     as S
import           Test.QuickCheck
import           Text.Read

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
data UGraph v e = UGraph
    { _size :: Int
    , unUGraph :: HM.HashMap v (Links v e)
    } deriving (Eq, Generic)

instance (Hashable v, Eq v, Show v, Show e) => Show (UGraph v e) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . shows (toList m)

instance (Hashable v, Eq v, Read v, Read e) => Read (UGraph v e) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        return (fromList xs)

instance (NFData v, NFData e) => NFData (UGraph v e)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (UGraph v e) where
    arbitrary = insertEdges <$> arbitrary <*> pure empty

instance Graph UGraph where
    empty = UGraph 0 HM.empty
    order (UGraph _ g) = HM.size g
    size (UGraph s _) = s
    vertices (UGraph _ g) = HM.keys g
    edgePairs g = toPair <$> edges g

    containsVertex (UGraph _ g) = flip HM.member g
    areAdjacent (UGraph _ g) v1 v2 = HM.member v2 $ getLinks v1 g
    adjacentVertices (UGraph _ g) v = HM.keys $ getLinks v g
    directlyReachableVertices g v = v : (adjacentVertices g v)
    vertexDegree (UGraph _ g) v = length $ HM.keys $ getLinks v g
    insertVertex v (UGraph s g) = UGraph s $ hashMapInsert v HM.empty g

    containsEdgePair = containsEdge'
    incidentEdgePairs g v = fmap toPair $ incidentEdges g v
    insertEdgePair (v1, v2) g = insertEdge (Edge v1 v2 ()) g
    removeEdgePair = removeEdge'

    removeVertex v g@(UGraph s _) = UGraph s
        $ (\(UGraph _ g') -> HM.delete v g')
        $ foldl' (flip removeEdge) g $ incidentEdges g v

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && (not $ HM.member v $ getLinks v $ unUGraph g)

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



-- | @O(log n)@ Insert an undirected 'Edge' into a 'UGraph'
-- | The involved vertices are inserted if don't exist. If the graph already
-- | contains the Edge, its attribute is updated
insertEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
insertEdge (Edge v1 v2 edgeAttr) g@(UGraph s _)
    | containsEdgePair g (v1, v2) = g
    | otherwise = UGraph (s + 1) $ link v2 v1 $ link v1 v2 g'
    where
        g' = unUGraph $ insertVertices [v1, v2] g
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | @O(m*log n)@ Insert many directed 'Edge's into a 'UGraph'
-- | Same rules as 'insertEdge' are applied
insertEdges :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e -> UGraph v e
insertEdges es g = foldl' (flip insertEdge) g es

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph' if present
-- | The involved vertices are left untouched
removeEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdge = removeEdgePair . toPair

-- | Same as 'removeEdge' but the edge is an unordered pair
removeEdge' :: (Hashable v, Eq v) => (v, v) -> UGraph v e -> UGraph v e
removeEdge' (v1, v2) graph@(UGraph s g)
    | containsEdgePair graph (v1, v2) =
        UGraph (s - 1) $ update v2Links v2 $ update v1Links v1 g
    | otherwise = graph
    where
        v1Links = HM.delete v2 $ getLinks v1 g
        v2Links = HM.delete v1 $ getLinks v2 g
        update = HM.adjust . const

-- | @O(log n)@ Remove the undirected 'Edge' from a 'UGraph' if present
-- | The involved vertices are also removed
removeEdgeAndVertices :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdgeAndVertices = removeEdgePairAndVertices . toPair

-- | @O(n*m)@ Retrieve the 'Edge's of a 'UGraph'
edges :: forall v e . (Hashable v, Eq v) => UGraph v e -> [Edge v e]
edges g = F.toList $ go g S.empty
    where
        go (order -> 0) es = es
        go g' es =
            let v = head $ vertices g'
            in go
                (removeVertex v g')
                (es S.>< (S.fromList $ incidentEdges g' v))

-- | @O(log n)@ Tell if an undirected 'Edge' exists in the graph
containsEdge :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> Bool
containsEdge g = containsEdge' g . toPair

-- | Same as 'containsEdge' but the edge is an unordered pair
containsEdge' :: (Hashable v, Eq v) => UGraph v e -> (v, v) -> Bool
containsEdge' graph@(UGraph _ g) (v1, v2) =
    containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
    where v1Links = getLinks v1 g

-- | Retrieve the incident 'Edge's of a Vertex
incidentEdges :: (Hashable v, Eq v) => UGraph v e -> v -> [Edge v e]
incidentEdges (UGraph _ g) v = fmap (uncurry (Edge v)) (HM.toList (getLinks v g))


-- * Lists

-- | Convert a 'UGraph' to a list of 'Edge's
-- | Same as 'edges'
toList :: (Hashable v, Eq v) => UGraph v e -> [Edge v e]
toList = edges

-- | Construct a 'UGraph' from a list of 'Edge's
fromList :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e
fromList es = insertEdges es empty
