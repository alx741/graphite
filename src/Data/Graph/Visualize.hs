module Data.Graph.Visualize
    ( plotUGraph
    , plotUGraphPng

    , plotDGraph
    , plotDGraphPng
    ) where

import Control.Concurrent

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Hashable

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Plot an undirected 'UGraph'
plotUGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> IO ThreadId
plotUGraph g = forkIO $ runGraphvizCanvas Sfdp (toUndirectedDot g) Xlib

-- | Plot an undirected 'UGraph' to a PNG image file
plotUGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> FilePath
 -> IO FilePath
plotUGraphPng g = addExtension (runGraphvizCommand Sfdp $ toUndirectedDot g) Png

-- | Plot a directed 'DGraph'
plotDGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> IO ThreadId
plotDGraph g = forkIO $ runGraphvizCanvas Sfdp (toDirectedDot g) Xlib

-- | Plot a directed 'DGraph' to a PNG image file
plotDGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> FilePath
 -> IO FilePath
plotDGraphPng g = addExtension (runGraphvizCommand Sfdp $ toDirectedDot g) Png

labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> vertices g

labeledEdges :: (Hashable v, Eq v, Show e) => UGraph v e -> [(v, v, String)]
labeledEdges g = (\(Edge v1 v2 attr) -> (v1, v2, show attr)) <$> edges g

labeledArcs :: (Hashable v, Eq v, Show e) => DGraph v e -> [(v, v, String)]
labeledArcs g = (\(Arc v1 v2 attr) -> (v1, v2, show attr)) <$> arcs g

toUndirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => UGraph v e
 -> DotGraph v
toUndirectedDot g = graphElemsToDot params (labeledNodes g) (labeledEdges g)
    where params = nonClusteredParams
            { isDirected = False
            , globalAttributes = [GraphAttrs [Overlap ScaleOverlaps]]
            }

toDirectedDot :: (Hashable v, Ord v, Show v, Show e) => DGraph v e -> DotGraph v
toDirectedDot g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = nonClusteredParams
            { isDirected = True
            , globalAttributes = [GraphAttrs [Overlap ScaleOverlaps]]
            }
