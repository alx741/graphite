{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Types where

import Data.List (nub)
import GHC.Float (float2Double)

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Test.QuickCheck

-- | Undirected Edge with attribute of type /e/ between to Vertices of type /v/
data Edge v e = Edge v v e
    deriving (Show, Read, Ord)

-- | Directed Arc with attribute of type /e/ between to Vertices of type /v/
data Arc v e = Arc v v e
    deriving (Show, Read, Ord)

-- | Each vertex maps to a 'Links' value so it can poit to other vertices
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

instance (Arbitrary v, Arbitrary e, Num v, Ord v) => Arbitrary (Edge v e) where
    arbitrary = arbitraryEdge Edge

instance (Arbitrary v, Arbitrary e, Num v, Ord v) => Arbitrary (Arc v e) where
    arbitrary = arbitraryEdge Arc

-- | Edges generator
arbitraryEdge :: (Arbitrary v, Arbitrary e, Ord v, Num v)
    => (v -> v -> e -> edge)
    -> Gen edge
arbitraryEdge edgeType = edgeType <$> vert <*> vert <*> arbitrary
    where vert = getPositive <$> arbitrary

-- | Weighted Edge attributes
-- | Useful for computing some algorithms on graphs
class Weighted a where
    weight :: a -> Double

-- | Labeled Edge attributes
-- | Useful for graph plotting
class Labeled a where
    label :: a -> String

instance Weighted Int where
    weight = fromIntegral

instance Weighted Float where
    weight = float2Double

instance Weighted Double where
    weight = id

instance Labeled String where
    label = id

instance Weighted (Double, String) where
    weight = fst

instance Labeled (Double, String) where
    label = snd

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
linksToEdges :: (Eq v, Eq a) => [(v, Links v a)] -> [Edge v a]
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
