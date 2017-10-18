module Data.Graph.Visualize
    ( plotUGraph
    , plotUGraphPng
    , plotDGraph
    , plotDGraphPng
    , plotUGraphEdgeLabeled
    , plotDGraphEdgeLabeled

    , labeledNodes
    , labeledEdges
    ) where

import Control.Concurrent

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.Hashable
import qualified Data.Text.Lazy                    as TL

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Plot an undirected 'UGraph'
plotUGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> IO ThreadId
plotUGraph g = forkIO $ runGraphvizCanvas Sfdp (toUndirectedDot False g) Xlib

-- | Same as 'plotUGraph' but render edge labels
plotUGraphEdgeLabeled :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> IO ThreadId
plotUGraphEdgeLabeled g = forkIO $ runGraphvizCanvas Sfdp (toUndirectedDot True g) Xlib

-- | Plot an undirected 'UGraph' to a PNG image file
plotUGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> FilePath
 -> IO FilePath
plotUGraphPng g = addExtension (runGraphvizCommand Sfdp $ toUndirectedDot False g) Png

-- | Plot a directed 'DGraph'
plotDGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> IO ThreadId
plotDGraph g = forkIO $ runGraphvizCanvas Sfdp (toDirectedDot False g) Xlib

-- | Same as 'plotDGraph' but render edge labels
plotDGraphEdgeLabeled :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> IO ThreadId
plotDGraphEdgeLabeled g = forkIO $ runGraphvizCanvas Sfdp (toDirectedDot True g) Xlib

-- | Plot a directed 'DGraph' to a PNG image file
plotDGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> FilePath
 -> IO FilePath
plotDGraphPng g = addExtension (runGraphvizCommand Sfdp $ toDirectedDot False g) Png

labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> vertices g

labeledEdges :: (Hashable v, Eq v, Show e) => UGraph v e -> [(v, v, String)]
labeledEdges g = (\(Edge v1 v2 attr) -> (v1, v2, show attr)) <$> edges g

labeledArcs :: (Hashable v, Eq v, Show e) => DGraph v e -> [(v, v, String)]
labeledArcs g = (\(Arc v1 v2 attr) -> (v1, v2, show attr)) <$> arcs g

toUndirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => Bool -- ^ Label edges
 -> UGraph v e
 -> DotGraph v
toUndirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledEdges g)
    where params = sensibleDotParams False labelEdges

toDirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => Bool -- ^ Label edges
 -> DGraph v e
 -> DotGraph v
toDirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = sensibleDotParams True labelEdges

sensibleDotParams
 :: Bool -- ^ Directed
 -> Bool -- ^ Label edges
 -> GraphvizParams t l String () l
sensibleDotParams directed edgeLabeled = nonClusteredParams
    { isDirected = directed
    , globalAttributes =
        [ GraphAttrs [Overlap ScaleOverlaps]
        , EdgeAttrs [FontColor (X11Color DarkGreen)]
        ]
    , fmtEdge = edgeFmt
    }
    where
        edgeFmt (_, _, l) = if edgeLabeled
            then [Label $ StrLabel $ TL.pack l]
            else []
