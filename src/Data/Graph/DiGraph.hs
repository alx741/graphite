{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Graph.DiGraph where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

import Data.Graph.Types

myGraph :: DiGraph Int (Double, String)
myGraph = insertArc (Arc 1 2 (55.5, "label")) empty


type DiGraph v e = HM.HashMap v (Links v e)

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
