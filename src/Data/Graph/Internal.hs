module Data.Graph.Internal where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

import Data.Graph.Types

-- | Each vertex maps to a 'Links' value so it can poit to other vertices
type Links v e = HM.HashMap v e

-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink = HM.insert

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> HM.HashMap v (Links v e) -> Links v e
getLinks = HM.lookupDefault HM.empty

-- | Get 'Arc's from an association list of vertices and their links
linksToArcs :: [(v, Links v a)] -> [Arc v a]
linksToArcs = concatMap toArc
    where
        toArc :: (v, Links v a) -> [Arc v a]
        toArc (fromV, links) = fmap (uncurry (Arc fromV)) (HM.toList links)

-- | Get 'Edge's from an association list of vertices and their links
linksToEdges :: [(v, Links v a)] -> [Edge v a]
linksToEdges = concatMap toEdge
    where
        toEdge :: (v, Links v a) -> [Edge v a]
        toEdge (fromV, links) = fmap (uncurry (Edge fromV)) (HM.toList links)

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m
