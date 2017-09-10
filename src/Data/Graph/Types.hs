{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Graph.Types where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Data.List         (nub)

-- | Undirected Edge with attribute of type /e/ between to Vertices of type /v/
data Edge v e = Edge v v e
    deriving (Show)

-- | Directed Arc with attribute of type /e/ between to Vertices of type /v/
data Arc v e = Arc v v e
    deriving (Show)

type Links v e = HM.HashMap v e

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


-- | Edges Attribute types migh expose a Weight (useful for computing some
-- | algorithms on graphs) and a Label (useful for graph plotting)
class EdgeAttr a where
    edgeWeight :: a -> Maybe Double
    edgeLabel :: a -> Maybe String

-- | Unit Edge Attribute. Useful when no attribute is neded
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

-- | Construct an undirected 'Edge' between two vertices
(<->) :: (Hashable v) => v -> v -> Edge v ()
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct a directed 'Arc' between two vertices
(-->) :: (Hashable v) => v -> v -> Arc v ()
(-->) v1 v2 = Arc v1 v2 ()

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

-- | Get the vertices of an 'Arc' ignoring its attribute
arcToTuple :: Arc v a -> (v, v)
arcToTuple (Arc fromV toV _) = (fromV, toV)

-- | Get the vertices of an 'Edge' ignoring its attribute
edgeToTuple :: Edge v a -> (v, v)
edgeToTuple (Edge v1 v2 _) = (v1, v2)

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
