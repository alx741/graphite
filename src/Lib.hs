{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lib where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

data Edge v a
    = Edge v v a
    -- | WeightedEdge Double v v a
    deriving (Eq, Show)

data Arc v a
    = Arc v v a
    -- | WeightedArc Double v v a
    deriving (Eq, Show)

class EdgeAttr a where
    edgeWeight :: a -> Maybe Double
    edgeLabel :: a -> Maybe String

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

type DiGraph v a = HM.HashMap v (Links v a)
type Links v a = HM.HashMap v a

empty :: (Hashable v) => DiGraph v e
empty = HM.empty

insertVertex :: (Hashable v, Eq v) => v -> DiGraph v a -> DiGraph v a
insertVertex v = hashMapInsert v HM.empty

insertVertices :: (Hashable v, Eq v) => [v] -> DiGraph v a -> DiGraph v a
insertVertices [] g     = g
insertVertices (v:vs) g = insertVertices vs $ insertVertex v g

insertArc :: (Hashable v, Eq v) => Arc v a -> DiGraph v a -> DiGraph v a
insertArc (Arc fromV toV attr) g = HM.adjust (insertLink toV attr) fromV g'
    where g' = insertVertices [fromV, toV] g

-- | Retrieve the vertices of a 'DiGraph'
vertices :: DiGraph v a -> [v]
vertices = HM.keys

-- | Retrieve the 'Arc's of a 'DiGraph'
arcs :: forall v a . (Hashable v, Eq v) => DiGraph v a -> [Arc v a]
arcs g = linksToArcs $ zip vs links
    where
        vs :: [v]
        vs = vertices g
        links :: [Links v a]
        links = fmap (\v -> getLinks v g) vs

-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink v attr m = HM.insert v attr m

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> DiGraph v a -> Links v a
getLinks = HM.lookupDefault HM.empty

-- | Get 'Arc's from an association list of vertices and their links
linksToArcs :: [(v, Links v a)] -> [Arc v a]
linksToArcs ls = concat $ fmap toArc ls
    where
        toArc :: (v, Links v a) -> [Arc v a]
        toArc (fromV, links) = fmap (\(v, a) -> Arc fromV v a) (HM.toList links)


myGraph :: DiGraph Int (Double, String)
myGraph = insertArc (Arc 1 2 (55.5, "label")) empty

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
