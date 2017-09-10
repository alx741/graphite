{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lib where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Data.List         (nub)

data Edge v a
    = Edge v v a
    -- | WeightedEdge Double v v a
    deriving (Show)

data Arc v a
    = Arc v v a
    -- | WeightedArc Double v v a
    deriving (Show)

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


class EdgeAttr a where
    edgeWeight :: a -> Maybe Double
    edgeLabel :: a -> Maybe String

instance EdgeAttr () where
    edgeWeight _ = Nothing
    edgeLabel _ = Nothing

instance EdgeAttr Double where
    edgeWeight v = Just v
    edgeLabel _ = Nothing

instance EdgeAttr String where
    edgeWeight _ = Nothing
    edgeLabel l = Just l

instance EdgeAttr (Double, String) where
    edgeWeight = Just . fst
    edgeLabel = Just . snd

-- | Construct a 'SimpleEdge' for two vertices
(<->) :: (Hashable v) => v -> v -> Edge v ()
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct a 'SimpleArc' for two vertices
(-->) :: (Hashable v) => v -> v -> Arc v ()
(-->) v1 v2 = Arc v1 v2 ()

type DiGraph v e = HM.HashMap v (Links v e)
type Links v e = HM.HashMap v e

empty :: (Hashable v) => DiGraph v e
empty = HM.empty

insertVertex :: (Hashable v, Eq v) => v -> DiGraph v e -> DiGraph v e
insertVertex v = hashMapInsert v HM.empty

insertVertices :: (Hashable v, Eq v) => [v] -> DiGraph v e -> DiGraph v e
insertVertices [] g     = g
insertVertices (v:vs) g = insertVertices vs $ insertVertex v g

insertArc :: (Hashable v, Eq v) => Arc v e -> DiGraph v e -> DiGraph v e
insertArc (Arc fromV toV edgeAttr) g = HM.adjust (insertLink toV edgeAttr) fromV g'
    where g' = insertVertices [fromV, toV] g

insertArcs :: (Hashable v, Eq v) => [Arc v e] -> DiGraph v e -> DiGraph v e
insertArcs [] g     = g
insertArcs (a:as) g = insertArcs as $ insertArc a g

-- | Retrieve the vertices of a 'DiGraph'
vertices :: DiGraph v e -> [v]
vertices = HM.keys

-- | Retrieve the 'Arc's of a 'DiGraph'
arcs :: forall v e . (Hashable v, Eq v, Eq e) => DiGraph v e -> [Arc v e]
arcs g = linksToArcs $ zip vs links
    where
        vs :: [v]
        vs = vertices g
        links :: [Links v e]
        links = fmap (\v -> getLinks v g) vs

-- | Retrieve the 'Arc's of a 'DiGraph' as tuples, ignoring its attributes
arcs' :: (Hashable v, Eq v, Eq e) => DiGraph v e -> [(v, v)]
arcs' g = fmap arcToTuple $ arcs g

-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink v edgeAttr m = HM.insert v edgeAttr m

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> DiGraph v e -> Links v e
getLinks = HM.lookupDefault HM.empty


-- | Get 'Arc's from an association list of vertices and their links
linksToArcs :: [(v, Links v a)] -> [Arc v a]
linksToArcs ls = concat $ fmap toArc ls
    where
        toArc :: (v, Links v a) -> [Arc v a]
        toArc (fromV, links) = fmap (\(v, a) -> Arc fromV v a) (HM.toList links)

-- | Get 'Edge's from an association list of vertices and their links
linksToEdges :: (Eq v, Eq a)  =>  [(v, Links v a)] -> [Edge v a]
linksToEdges ls = nub $ concat $ fmap toEdge ls
    where
        toEdge :: (v, Links v a) -> [Edge v a]
        toEdge (fromV, links) = fmap (\(v, a) -> Edge fromV v a) (HM.toList links)

-- | Get the vertices of an 'Arc' ignoring its attributes
arcToTuple :: Arc v a -> (v, v)
arcToTuple (Arc fromV toV _) = (fromV, toV)

-- | Get the vertices of an 'Edge' ignoring its attributes
edgeToTuple :: Edge v a -> (v, v)
edgeToTuple (Edge v1 v2 _) = (v1, v2)

myGraph :: DiGraph Int (Double, String)
myGraph = insertArc (Arc 1 2 (55.5, "label")) empty

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
