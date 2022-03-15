{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DGraph
    (
    -- * DGraph data type
    DGraph

    -- * Functions on DGraph
    , insertArc
    , insertArcs
    , removeArc
    , removeArcs
    , removeArcAndVertices
    , arcs
    , containsArc
    , inboundingArcs
    , outboundingArcs
    , incidentArcs
    , vertexIndegree
    , vertexOutdegree
    , indegrees
    , outdegrees
    -- ** Query graph properties and characteristics
    -- , isSymmetric
    -- , isOriented
    , isBalanced
    -- , isRegular
    , isSource
    , isSink
    , isInternal
    -- ** Transformations
    , transpose
    , toUndirected

    -- * List conversions
    , toArcsList
    , fromArcsList

    -- * Pretty printing
    , prettyPrint
    ) where

import Data.List      (foldl', intersect)
import Data.Semigroup
import GHC.Generics   (Generic)

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck
import           Text.Read

import           Data.Graph.Internal
import           Data.Graph.Types
import qualified Data.Graph.UGraph   as UG

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

instance (Hashable v, Eq v) => Monoid (DGraph v e) where
    mempty = empty
    mappend = union

instance (Hashable v, Eq v) => Semigroup (DGraph v e) where
    (<>) = mappend

instance (Hashable v, Eq v) => Functor (DGraph v) where
    fmap f (DGraph s g) = DGraph s $ fmap (fmap f) g

instance (Hashable v, Eq v) => Foldable (DGraph v) where
    foldMap f g = foldMap f $ fmap attribute $ arcs g
    foldr f acc g = foldr f acc $ fmap attribute $ arcs g

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
        (toTriple <$> toArcsList g)

    reachableAdjacentVertices (DGraph _ g) v = HM.keys (getLinks v g)

    reachableAdjacentVertices' g v = filter
        (\(fromV, _, _) -> fromV == v)
        (toTriple <$> toArcsList g)

    -- | The total number of inbounding and outbounding 'Arc's of a vertex
    vertexDegree g v = vertexIndegree g v + vertexOutdegree g v

    insertVertex v (DGraph s g) = DGraph s $ hashMapInsert v HM.empty g

    containsEdgePair (DGraph _ g) (v1, v2) = v2 `HM.member` (getLinks v1 g)

    incidentEdgeTriples g v = toTriple <$> incidentArcs g v
    insertEdgeTriple (v1, v2, e) = insertArc (Arc v1 v2 e)

    removeEdgePair (v1, v2) graph@(DGraph s g)
        | containsEdgePair graph (v1, v2) =
            DGraph (s - 1) $ HM.adjust (const v1Links') v1 g
        | otherwise = graph
            where v1Links' = HM.delete v2 $ getLinks v1 g

    removeVertex v g = DGraph s $ HM.delete v g'
        where (DGraph s g') = foldl' (flip removeArc) g $ incidentArcs g v

    isSimple g = foldl' go True $ vertices g
        where go bool v = bool && not (HM.member v $ getLinks v $ unDGraph g)


    union g1 g2 = insertArcs (toArcsList g1) $ insertVertices (vertices g1) g2

    intersection g1 g2 =
        insertVertices (isolatedVertices g1 `intersect` isolatedVertices g2) $
        fromArcsList (toArcsList g1 `intersect` toArcsList g2)



    toList (DGraph _ g) = zip vs $ fmap (\v -> HM.toList $ getLinks v g) vs
        where vs = HM.keys g

    fromAdjacencyMatrix m
        | length m /= length (head m) = Nothing
        | otherwise = Just $ insertArcs (foldl' genArcs [] labeledM) empty
            where
                labeledM :: [(Int, [(Int, Int)])]
                labeledM = zip [1..] $ fmap (zip [1..]) m

                genArcs :: [Arc Int ()] -> (Int, [(Int, Int)]) -> [Arc Int ()]
                genArcs as (i, vs) = as ++ fmap (\v -> Arc i v ()) connected
                    where connected = fst <$> filter (\(_, v) -> v /= 0) vs

    -- toAdjacencyMatrix = undefined

instance (Arbitrary v, Arbitrary e, Hashable v, Num v, Ord v)
 => Arbitrary (DGraph v e) where
    arbitrary = insertArcs <$> arbitrary <*> pure empty

-- | Insert a directed 'Arc' into a 'DGraph'
--
-- The involved vertices are inserted if they don't exist. If the graph
-- already contains the Arc, its attribute gets updated
insertArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
insertArc (Arc fromV toV edgeAttr) g@(DGraph s _)
    | containsEdgePair g (fromV, toV) =  DGraph s $ HM.adjust (insertLink toV edgeAttr) fromV $ unDGraph g
    | otherwise = DGraph (s + 1) $ HM.adjust (insertLink toV edgeAttr) fromV $ unDGraph g'
    where g' = insertVertices [fromV, toV] g

-- | Same as 'insertArc' but for a list of 'Arc's
insertArcs :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e -> DGraph v e
insertArcs as g = foldl' (flip insertArc) g as

-- | Remove the directed 'Arc' from a 'DGraph' if present. The involved vertices
-- are left untouched
removeArc :: (Hashable v, Eq v) => Arc v e -> DGraph v e -> DGraph v e
removeArc = removeEdgePair . toPair

-- | Same as 'removeArc' but for a list of 'Arc's
removeArcs :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e -> DGraph v e
removeArcs as g = foldl' (flip removeArc) g as

-- | Remove the directed 'Arc' from a 'DGraph' if present. The involved vertices
-- also get removed
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
--
-- The @incident@ arcs of a vertex are all the inbounding and outbounding arcs
-- of the vertex
incidentArcs :: (Hashable v, Eq v) => DGraph v e -> v -> [Arc v e]
incidentArcs g v = inboundingArcs g v ++ outboundingArcs g v

-- | Indegree of a vertex
--
-- The @indegree@ of a vertex is the number of inbounding 'Arc's to a vertex
vertexIndegree :: (Hashable v, Eq v) => DGraph v e -> v -> Int
vertexIndegree g v = length $ filter (\(_, v') -> v == v' ) $ edgePairs g

-- | Outdegree of a vertex
--
-- The @outdegree@ of a vertex is the number of outbounding 'Arc's from a vertex
vertexOutdegree :: (Hashable v, Eq v) => DGraph v e -> v -> Int
vertexOutdegree g v = length $ filter (\(v', _) -> v == v' ) $ edgePairs g

-- | Indegrees of all the vertices in a 'DGraph'
indegrees :: (Hashable v, Eq v) => DGraph v e -> [Int]
indegrees g = vertexIndegree g <$> vertices g

-- | Outdegree of all the vertices in a 'DGraph'
outdegrees :: (Hashable v, Eq v) => DGraph v e -> [Int]
outdegrees g = vertexOutdegree g <$> vertices g

-- | Tell if a 'DGraph' is symmetric
--
-- A directed graph is @symmetric@ if all of its 'Arc's are bi-directed
isSymmetric :: DGraph v e -> Bool
isSymmetric = undefined

-- | Tell if a 'DGraph' is oriented
--
-- A directed graph is @oriented@ if there are none bi-directed 'Arc's
--
-- Note: This is /not/ the opposite of 'isSymmetric'
isOriented :: DGraph v e -> Bool
isOriented = undefined

-- | Tell if a 'DGraph' is balanced
--
-- A directed graph is @balanced@ when its @indegree = outdegree@
isBalanced :: (Hashable v, Eq v) => DGraph v e -> Bool
isBalanced g = sum (indegrees g) == sum (outdegrees g)

-- | Tell if a 'DGraph' is regular
--
-- A directed graph is @regular@ when all of its vertices have the same number
-- of adjacent vertices /AND/ when the @indegree@ and @outdegree@ of each vertex
-- are equal to each other.
isRegular :: DGraph v e -> Bool
isRegular _ = undefined

-- | Tell if a vertex is a source
--
-- A vertex is a @source@ when its @indegree = 0@
isSource :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isSource g v = vertexIndegree g v == 0

-- | Tell if a vertex is a sink
--
-- A vertex is a @sink@ when its @outdegree = 0@
isSink :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isSink g v = vertexOutdegree g v == 0

-- | Tell if a vertex is internal
--
-- A vertex is @internal@ when its neither a @source@ nor a @sink@
isInternal :: (Hashable v, Eq v) => DGraph v e -> v -> Bool
isInternal g v = not $ isSource g v || isSink g v


-- | Get the transpose of a 'DGraph'
--
-- The @transpose@ of a directed graph is another directed graph where all of
-- its arcs are reversed
transpose :: (Hashable v, Eq v) => DGraph v e -> DGraph v e
transpose g = insertArcs (reverseArc <$> arcs g) empty
    where reverseArc (Arc fromV toV attr) = Arc toV fromV attr

-- | Convert a directed 'DGraph' to an undirected 'UGraph' by converting all of
-- its 'Arc's into 'Edge's
toUndirected :: (Hashable v, Eq v) => DGraph v e -> UG.UGraph v e
toUndirected g = UG.insertEdges (arcToEdge <$> arcs g) empty
    where arcToEdge (Arc fromV toV attr) = Edge fromV toV attr


-- | Convert a 'DGraph' to a list of 'Arc's discarding isolated vertices
--
-- Note that because 'toArcsList' discards isolated vertices:
--
-- > fromArcsList . toArcsList /= id
toArcsList :: (Hashable v, Eq v) => DGraph v e -> [Arc v e]
toArcsList = arcs

-- | Construct a 'DGraph' from a list of 'Arc's
fromArcsList :: (Hashable v, Eq v) => [Arc v e] -> DGraph v e
fromArcsList as = insertArcs as empty


-- | Pretty print a 'DGraph'
prettyPrint :: (Hashable v, Eq v, Show v, Show e) => DGraph v e -> String
prettyPrint g =
    "Isolated Vertices: "
    <> show (filter (\v -> vertexDegree g v == 0) $ vertices g)
    <> "   "
    <> "Arcs: "
    <> show (arcs g)
