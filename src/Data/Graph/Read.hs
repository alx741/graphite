{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Read where

import Data.ByteString.Lazy as BS hiding (empty)
import Data.Csv             as CSV
import Data.Hashable
import Data.Vector          as V hiding (empty, fromList)

import Data.Graph.Types

-- | Read a 'UGraph' from a CSV file
-- | The line "1,2,3,4" translates to the list of edges
-- | "(1 <-> 2), (1 <-> 3), (1 <-> 4)"
fromCsv :: Graph g => (Hashable v, Eq v, FromField v)
 => FilePath
 -> IO (Either String (g v ()))
fromCsv fp = do
    content <- BS.readFile fp
    let dec = decode NoHeader content
    case dec of
        Left err -> return $ Left err
        Right vec -> return $ Right $ flip insertEdgePairs empty $ toEdges $ V.toList vec

    where
        toEdges :: [[v]] -> [(v, v)]
        toEdges = Prelude.concatMap nodeEdges

        nodeEdges :: [v] -> [(v, v)]
        nodeEdges []     = []
        nodeEdges (n:ns) = fmap (\n' -> (n, n')) ns


-- | Same as 'fromCsv' but rise an exception when parsing fails
fromCsv' :: Graph g => (Hashable v, Eq v, FromField v)
 => FilePath
 -> IO (g v ())
fromCsv' fp = do
    eitherG <- fromCsv fp
    case eitherG of
        Left err -> error err
        Right g  -> return g
