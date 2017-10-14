{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DGraph where

import Data.List    (foldl')
import GHC.Generics (Generic)

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck
import           Text.Read

import           Data.Graph.Types
import qualified Data.Graph.UGraph as UG

-- | Directed Graph of Vertices in /v/ and Arcs with attributes in /e/
data DGraph v e = DGraph
    { _size    :: Int
    , unDGraph :: HM.HashMap v (Links v e)
    } deriving (Eq, Generic)

instance (Hashable v, Eq v, Show v, Show e) => Show (DGraph v e) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . shows (toList m)

instance (Hashable v, Eq v, Read v, Read e) => Read (DGraph v e) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        return (fromList xs)

instance (NFData v, NFData e) => NFData (DGraph v e)

instance Graph DGraph where
    empty = DGraph 0 HM.empty
    order (DGraph _ g) = HM.size g
    size (DGraph s _) = s
    vertices (DGraph _ g) = HM.keys g
    edgeTriples g = toTriple <$> arcs g

    edgeTriple (DGraph _ g) v1 v2 =
        let mAttr = HM.lookup v2 $ getLinks v1 g
        in case mAttr of
            Just attr -> Just (v1, v2, attr)
            Nothing   -> Nothing

    containsVertex (DGraph _ g) = flip HM.member g

    areAdjacent (DGraph _ g) v1 v2 =
        HM.member v2 (getLinks v1 g) || HM.member v1 (getLinks v2 g)

    adjacentVertices g v = filter
        (\v' -> containsEdgePair g (v, v') || containsEdgePair g (v', v))
        (vertices g)

    adjacentVertices' g v = filter
        (\(fromV, toV, _) -> fromV == v || toV == v)
        (toTriple <$> toList g)

    reachableAdjacentVertices (DGraph _ g) v = HM.keys (getLinks v g)

    reachableAdjacentVertices' g v = filter
        (\(fromV, _, _) -> fromV == v)
        (toTriple <$> toList g)

    -- | The total number of inbounding and outbounding 'Arc's of a vertex
    vertexDegree g v = vertexIndegree g v + vertexOutdegree g v

    insertVertex v (DGraph s g) = DGraph s $ hashMapInsert v HM.empty g

    containsEdgePair graph@(DGraph _ g) (v1, v2) =
        containsVertex graph v1 && containsVertex graph v2 && v2 `HM.member` v1Links
        where v1Links = getLinks v1 g


    incidentEdgeTriples g v = toTriple <$> incidentArcs g v
    insertEdgeTriple (v1, v2, e) = insertArc (Arc v1 v2 e)

    removeEdgePair (v1, v2) graph@(DGraph s g)
        | containsEdgePair graph (v1, v2) =
            DGraph (s - 1) $ HM.adjust (const v1Links') v1 g
        | otherwise = graph
            where v1Links' = HM.delete v2 $ getLinks v1 g


    removeVertex v g@(DGraph s _) = DGraph s
        $ (\(DGraph _ g') -> HM.delete v g')
        $ foldl' (flip removeArc) g $ incidentArcs g v

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && not (HM.member v $ getLinks v $ unDGraph g)

    fromAdjacencyMatrix m
        | length m /= length (head m) = Nothing
        | otherwise = Just $ insertArcs (foldl' genArcs [] labeledM) empty
            where
                labeledM :: [(Int, [(Int, Int)])]
                labeledM = zip [1..] $ fmap (zip [1..]) m

                genArcs :: [Arc Int ()] -> (Int, [(Int, Int)]) -> [Arc Int ()]
                genArcs as (i, vs) = as ++ fmap (\v -> Arc i v ()) connected
                    where connected = fst <$> filter (\(_, v) -> v /= 0) vs

    toAdjacencyMatrix = undefined

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (DGraph v e) where
    arbitrary = insertArcs <$> arbitrary <*> pure empty

-- | Insert a directed 'Arc' into a 'DGraph'
-- | The involved vertices are inserted if they don't exist. If the graph
-- | already contains the Arc, its attribute is updated
insertArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
insertArc (Arc fromV toV edgeAttr) g@(DGraph s _)
    | containsEdgePair g (fromV, toV) = g
    | otherwise = DGraph (s + 1) $ HM.adjust (insertLink toV edgeAttr) fromV g'
    where g' = unDGraph $ insertVertices [fromV, toV] g

-- | Same as 'insertArc' but for a list of 'Arc's
insertArcs :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e -> DGraph v e
insertArcs as g = foldl' (flip insertArc) g as

-- | Remove the directed 'Arc' from a 'DGraph' if present
-- | The involved vertices are left untouched
removeArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
removeArc = removeEdgePair . toPair

-- | Same as 'removeArc' but for a list of 'Arc's
removeArcs :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e -> DGraph v e
removeArcs as g = foldl' (flip removeArc) g as

-- | Remove the directed 'Arc' from a 'DGraph' if present
-- | The involved vertices are also removed
removeArcAndVertices :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
removeArcAndVertices = removeEdgePairAndVertices . toPair

-- | Retrieve the 'Arc's of a 'DGraph'
arcs :: forall v e . (Hashable v, Eq v) => DGraph v e -> [Arc v e]
arcs (DGraph s g) = linksToArcs $ zip vs links
    where
        vs :: [v]
        vs = vertices $ DGraph s g
        links :: [Links v e]
        links = fmap (`getLinks` g) vs

-- | Tell if a directed 'Arc' exists in the graph
containsArc :: (Hashable v, Eq v) => DGraph v e -> Arc v e -> Bool
containsArc g = containsEdgePair g . toPair

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

-- | Tell if a 'DGraph' is symmetric
-- | All of its 'Arc's are bidirected
isSymmetric :: DGraph v e -> Bool
isSymmetric = undefined

-- | Tell if a 'DGraph' is oriented
-- | There are none bidirected 'Arc's
-- | Note: This is /not/ the opposite of 'isSymmetric'
isOriented :: DGraph v e -> Bool
isOriented = undefined

-- | Indegree of a vertex
-- | The number of inbounding 'Arc's to a vertex
vertexIndegree :: (Hashable v, Eq v) => DGraph v e -> v -> Int
vertexIndegree g v = length $ filter (\(_, v') -> v == v' ) $ edgePairs g

-- | Outdegree of a vertex
-- | The number of outbounding 'Arc's from a vertex
vertexOutdegree :: (Hashable v, Eq v) => DGraph v e -> v -> Int
vertexOutdegree g v = length $ filter (\(v', _) -> v == v' ) $ edgePairs g

-- | Indegrees of all the vertices in a 'DGraph'
indegrees :: (Hashable v, Eq v) => DGraph v e -> [Int]
indegrees g = vertexIndegree g <$> vertices g

-- | Outdegree of all the vertices in a 'DGraph'
outdegrees :: (Hashable v, Eq v) => DGraph v e -> [Int]
outdegrees g = vertexOutdegree g <$> vertices g

-- | Tell if a 'DGraph' is balanced
-- | A Directed Graph is @balanced@ when its @indegree = outdegree@
isBalanced :: (Hashable v, Eq v) => DGraph v e -> Bool
isBalanced g = sum (indegrees g) == sum (outdegrees g)

-- | Tell if a 'DGraph' is regular
-- | A Directed Graph is @regular@ when all of its vertices have the same number
-- | of adjacent vertices AND when the @indegree@ and @outdegree@ of each vertex
-- | are equal to each other.
isRegular :: DGraph v e -> Bool
isRegular _ = undefined

-- | Tell if a vertex is a source
-- | A vertex is a @source@ when its @indegree = 0@
isSource :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isSource g v = vertexIndegree g v == 0

-- | Tell if a vertex is a sink
-- | A vertex is a @sink@ when its @outdegree = 0@
isSink :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isSink g v = vertexOutdegree g v == 0

-- | Tell if a vertex is internal
-- | A vertex is a @internal@ when its neither a @source@ nor a @sink@
isInternal :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isInternal g v = not $ isSource g v || isSink g v

-- * Transformations

-- | Get the transpose of a 'DGraph'
-- | The @transpose@ of a directed graph is another directed graph where all of
-- | its arcs are reversed
transpose :: (Hashable v, Eq v) => DGraph v e -> DGraph v e
transpose g = insertArcs (reverseArc <$> arcs g) empty
    where reverseArc (Arc fromV toV attr) = Arc toV fromV attr

-- | Convert a directed 'DGraph' to an undirected 'UGraph' by converting all of
-- | its 'Arc's into 'Edge's
toUndirected :: (Hashable v, Eq v) => DGraph v e -> UG.UGraph v e
toUndirected g = UG.insertEdges (arcToEdge <$> arcs g) empty
    where arcToEdge (Arc fromV toV attr) = Edge fromV toV attr


-- * Lists

-- | Convert a 'DGraph' to a list of 'Arc's
-- | Same as 'arcs'
toList :: (Hashable v, Eq v) => DGraph v e -> [Arc v e]
toList = arcs

-- | Construct a 'DGraph' from a list of 'Arc's
fromList :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e
fromList as = insertArcs as empty
