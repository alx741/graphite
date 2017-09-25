module Data.Graph.UGraph.DegreeSequence where

import Data.List (reverse, sort)

import Data.Hashable

import Data.Graph.Types
import Data.Graph.UGraph

-- | The Degree Sequence of a simple 'UGraph' is a list of degrees of vertices
-- | in a graph
-- | Use 'degreeSequence' to construct a valid Degree Sequence
newtype DegreeSequence = DegreeSequence { unDegreeSequence :: [Int]}
    deriving (Eq, Ord, Show)

-- | Construct a 'DegreeSequence' from a list of degrees
-- | Negative degree values are discarded
degreeSequence :: [Int] -> DegreeSequence
degreeSequence = DegreeSequence . reverse . sort . filter (>0)

-- | Get the 'DegreeSequence' of a simple 'UGraph'
-- | If the graph is not @simple@ (see 'isSimple') the result is Nothing
getDegreeSequence :: (Hashable v, Eq v) => UGraph v e -> Maybe DegreeSequence
getDegreeSequence g
    | (not . isSimple) g = Nothing
    | otherwise = Just $ degreeSequence $ degrees g

-- | Tell if a 'DegreeSequence' is a Graphical Sequence
-- | A Degree Sequence is a @Graphical Sequence@ if a corresponding 'UGraph' for
-- | it exists
-- FIXME: the handshake lemma is not enough!
isGraphicalSequence :: DegreeSequence -> Bool
isGraphicalSequence ds = holdsHandshakingLemma ds && undefined

-- | Tell if a 'DegreeSequence' holds the Handshaking lemma, that is, if the
-- | number of vertices with odd degree is even
holdsHandshakingLemma :: DegreeSequence -> Bool
holdsHandshakingLemma = even . length . filter odd . unDegreeSequence

-- | Get the corresponding 'UGraph' of a 'DegreeSequence'
-- | If the 'DegreeSequence' is not graphical (see 'isGraphicalSequence') the
-- | result is Nothing
fromGraphicalSequence :: DegreeSequence -> Maybe (UGraph Int ())
fromGraphicalSequence = undefined
