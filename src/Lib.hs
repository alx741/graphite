{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

data Edge v a
    = Edge v v a
    | WeightedEdge Double v v a
    deriving (Eq, Show)

data Arc v a
    = Arc v v a
    | WeightedArc Double v v a
    deriving (Eq, Show)

-- -- | Simple, Unweighted Edge with no attributes
-- type SimpleEdge v = Edge v ()

-- -- | Simple, Unweighted Arc with no attributes
-- type SimpleArc v = Arc v ()

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

-- | Insert a link directed to *v* with attribute *a*
-- | If the connnection already exists, the attribute is replaced
insertLink :: (Hashable v, Eq v) => v -> a -> Links v a -> Links v a
insertLink v attr m = HM.insert v attr m

-- | Get the links for a given vertex
getLinks :: (Hashable v, Eq v) => v -> DiGraph v a -> Links v a
getLinks = HM.lookupDefault HM.empty

myGraph :: DiGraph Int ()
myGraph = empty

-- | O(log n) Associate the specified value with the specified key in this map.
-- | If this map previously contained a mapping for the key, leave the map
-- | intact.
hashMapInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
hashMapInsert k v m = if not (HM.member k m) then HM.insert k v m else m


-- insertArc :: (Vertex v, Arc a) => a -> DiGraph v e -> DiGraph v e
-- insertArc a g =
--     if HM.member from g
--     then undefined
--     else g
--     where
--         from = fromVertex a
--         to = toVertex a


-- -- mymap :: HM.HashMap Int String
-- -- mymap = HM.insert 1 "hola" HM.empty
