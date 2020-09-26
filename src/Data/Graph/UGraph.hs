{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Graph.UGraph
    (
    -- * UGraph data type
    UGraph

    -- * Functions on UGraph
    , insertEdge
    , insertEdges
    , removeEdge
    , removeEdges
    , removeEdgeAndVertices
    , edges
    , containsEdge
    , incidentEdges

    -- * List conversions
    , toEdgesList
    , fromEdgesList

    -- * Pretty printing
    , prettyPrint
    ) where

import qualified Data.Foldable  as F (toList)
import           Data.List      (foldl', intersect)
import           Data.Semigroup
import           GHC.Generics   (Generic)

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.Sequence     as S
import           Test.QuickCheck
import           Text.Read

import Data.Graph.Internal
import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
data UGraph v e = UGraph
    { _size    :: Int
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

instance (Hashable v, Eq v) => Monoid (UGraph v e) where
    mempty = empty
    mappend = union

instance (Hashable v, Eq v) => Semigroup (UGraph v e) where
    (<>) = mappend

instance (Hashable v, Eq v) => Functor (UGraph v) where
    fmap f (UGraph s g) = UGraph s $ fmap (fmap f) g

instance (Hashable v, Eq v) => Foldable (UGraph v) where
    foldMap f g = foldMap f $ fmap attribute $ edges g
    foldr f acc g = foldr f acc $ fmap attribute $ edges g

instance (NFData v, NFData e) => NFData (UGraph v e)

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (UGraph v e) where
    arbitrary = insertEdges <$> arbitrary <*> pure empty

instance Graph UGraph where
    empty = UGraph 0 HM.empty
    order (UGraph _ g) = HM.size g
    size (UGraph s _) = s
    vertices (UGraph _ g) = HM.keys g
    edgeTriples g = toTriple <$> edges g

    edgeTriple (UGraph _ g) v1 v2 =
        let mAttr = HM.lookup v2 $ getLinks v1 g
        in case mAttr of
            Just attr -> Just (v1, v2, attr)
            Nothing   -> Nothing

    containsVertex (UGraph _ g) = flip HM.member g
    areAdjacent (UGraph _ g) v1 v2 = HM.member v2 $ getLinks v1 g
    adjacentVertices (UGraph _ g) v = HM.keys $ getLinks v g
    adjacentVertices' (UGraph _ g) v = fmap (\(toV, e) -> (v, toV, e)) $
        HM.toList $ getLinks v g

    reachableAdjacentVertices = adjacentVertices
    reachableAdjacentVertices' = adjacentVertices'
    vertexDegree (UGraph _ g) v = length $ HM.keys $ getLinks v g
    insertVertex v (UGraph s g) = UGraph s $ hashMapInsert v HM.empty g

    containsEdgePair (UGraph _ g) (v1, v2) = v2 `HM.member` (getLinks v1 g)

    incidentEdgeTriples g v = toTriple <$> incidentEdges g v
    insertEdgeTriple (v1, v2, e) = insertEdge (Edge v1 v2 e)

    removeEdgePair (v1, v2) graph@(UGraph s g)
        | containsEdgePair graph (v1, v2) =
            UGraph (s - 1) $ update v2Links v2 $ update v1Links v1 g
        | otherwise = graph
        where
            v1Links = HM.delete v2 $ getLinks v1 g
            v2Links = HM.delete v1 $ getLinks v2 g
            update = HM.adjust . const

    removeVertex v g@(UGraph s _) = UGraph s
        $ (\(UGraph _ g') -> HM.delete v g')
        $ foldl' (flip removeEdge) g $ incidentEdges g v

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && not (HM.member v $ getLinks v $ unUGraph g)


    union g1 g2 = insertEdges (toEdgesList g1) $ insertVertices (vertices g1) g2

    intersection g1 g2 =
        insertVertices (isolatedVertices g1 `intersect` isolatedVertices g2) $
        fromEdgesList (toEdgesList g1 `intersect` toEdgesList g2)

    toList (UGraph _ g) = zip vs $ fmap (\v -> HM.toList $ getLinks v g) vs
        where vs = HM.keys g

    fromAdjacencyMatrix m
        | length m /= length (head m) = Nothing
        | otherwise = Just $ insertEdges (foldl' genEdges [] labeledM) empty
            where
                labeledM :: [(Int, [(Int, Int)])]
                labeledM = zip [1..] $ fmap (zip [1..]) m

                genEdges :: [Edge Int ()] -> (Int, [(Int, Int)]) -> [Edge Int ()]
                genEdges es (i, vs) = es ++ fmap (\v -> Edge i v ()) connected
                    where connected = fst <$> filter (\(_, v) -> v /= 0) vs

    -- toAdjacencyMatrix = undefined



-- | Insert an undirected 'Edge' into a 'UGraph'
--
-- The involved vertices are inserted if they don't exist. If the graph already
-- contains the Edge, its attribute gets updated
insertEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
insertEdge (Edge v1 v2 edgeAttr) g@(UGraph s _)
    -- | containsEdgePair g (v1, v2) = g
    | containsEdgePair g (v1, v2) = UGraph s $ link v2 v1 $ link v1 v2 $ unUGraph g
    | otherwise = UGraph (s + 1) $ link v2 v1 $ link v1 v2 $ unUGraph g'
    where
        g' = insertVertices [v1, v2] g
        link fromV toV = HM.adjust (insertLink toV edgeAttr) fromV

-- | Same as 'insertEdge' but for a list of 'Edge's
insertEdges :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e -> UGraph v e
insertEdges es g = foldl' (flip insertEdge) g es

-- | Remove the undirected 'Edge' from a 'UGraph' if present. The involved
-- vertices are left untouched
removeEdge :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdge = removeEdgePair . toPair

-- | Same as 'removeEdge' but for a list of 'Edge's
removeEdges :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e -> UGraph v e
removeEdges es g = foldl' (flip removeEdge) g es

-- | Remove the undirected 'Edge' from a 'UGraph' if present. The involved
-- vertices also get removed
removeEdgeAndVertices :: (Hashable v, Eq v) => Edge v e -> UGraph v e -> UGraph v e
removeEdgeAndVertices = removeEdgePairAndVertices . toPair

-- | Retrieve the 'Edge's of a 'UGraph'
edges :: forall v e . (Hashable v, Eq v) => UGraph v e -> [Edge v e]
edges g = F.toList $ go g S.empty
    where
        go (order -> 0) es = es
        go g' es =
            let v = head $ vertices g'
            in go
                (removeVertex v g')
                (es S.>< S.fromList (incidentEdges g' v))

-- | Tell if an undirected 'Edge' exists in the graph
containsEdge :: (Hashable v, Eq v) => UGraph v e -> Edge v e -> Bool
containsEdge g = containsEdgePair g . toPair

-- | Retrieve the incident 'Edge's of a Vertex
incidentEdges :: (Hashable v, Eq v) => UGraph v e -> v -> [Edge v e]
incidentEdges (UGraph _ g) v = fmap (uncurry (Edge v)) (HM.toList (getLinks v g))


-- | Convert a 'UGraph' to a list of 'Edge's discarding isolated vertices
--
-- Note that because 'toEdgesList' discards isolated vertices:
-- > fromEdgesList . toEdgesList /= id
toEdgesList :: (Hashable v, Eq v) => UGraph v e -> [Edge v e]
toEdgesList = edges

-- | Construct a 'UGraph' from a list of 'Edge's
fromEdgesList :: (Hashable v, Eq v) => [Edge v e] -> UGraph v e
fromEdgesList es = insertEdges es empty


-- | Pretty print a 'UGraph'
prettyPrint :: (Hashable v, Eq v, Show v, Show e) => UGraph v e -> String
prettyPrint g =
    "Isolated Vertices: "
    <> show (isolatedVertices g)
    <> "   "
    <> "Edges: "
    <> show (edges g)
