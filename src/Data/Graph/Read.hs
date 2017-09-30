{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Read where

import Data.ByteString.Lazy as BS
import Data.Csv             as CSV
import Data.Hashable
import Data.Vector          as V hiding (fromList)

import Data.Graph.Types
import Data.Graph.UGraph

-- | Read a 'UGraph' from a CSV file
-- | The line "1,2,3,4" translates to the list of edges
-- | "(1 <-> 2), (1 <-> 3), (1 <-> 4)"
csvToUGraph :: (Hashable v, Eq v, FromField v)
    => FilePath
    -> IO (Either String (UGraph v ()))
csvToUGraph fp = do
    content <- BS.readFile fp
    let dec = decode NoHeader content
    case dec of
        Left err  -> return $ Left err
        Right vec -> return $ Right $ fromList $ toEdges $ V.toList vec

    where
        toEdges :: [[v]] -> [Edge v ()]
        toEdges ns = Prelude.concat $ fmap nodeEdges ns

        nodeEdges :: [v] -> [Edge v ()]
        nodeEdges []     = []
        nodeEdges (n:ns) = fmap (\n' -> Edge n n' ()) ns
