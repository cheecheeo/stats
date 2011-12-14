{-# LANGUAGE OverloadedStrings #-}

-- cabal update && cabal install text statistics

import qualified Statistics.Quantile as S
import qualified Statistics.Sample as SS

import qualified Data.Function as F
import qualified Data.Char as C
import qualified Data.List as L
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Applicative ((<$>))
import qualified Control.Monad as M

newtype Quartiles = Quartiles (Double, Double, Double, Double, Double)
  deriving (Show)

quartiles :: Vector Double -> Quartiles
quartiles xs = Quartiles (V.minimum xs, go 1, go 2, go 3, V.maximum xs)
  where go quantile = S.weightedAvg quantile 4 xs

additionalStatistics :: Vector Double -> (Int, Double, Quartiles)
additionalStatistics xs = (V.length xs, SS.mean xs, quartiles xs)

headerAndAdditionalStatistics :: [(Text, Double)] -> (Text, (Int, Double, Quartiles))
headerAndAdditionalStatistics xs = (head headers, additionalStatistics (V.fromList sample))
  where (headers, sample) = unzip xs

tshow :: (Show a) => a -> Text
tshow = T.pack . show

printHeaderAndStats :: (Text, (Int, Double, Quartiles)) -> IO ()
printHeaderAndStats (header, (len, average, Quartiles (minSample, first, median, third, maxSample))) = do
  M.unless (T.strip header == "") (TIO.putStrLn header)
  TIO.putStrLn (T.append "Samples:         " (tshow len))
  TIO.putStrLn (T.append "Average:         " (tshow average))
  TIO.putStrLn (T.append "Minimum:         " (tshow minSample))
  TIO.putStrLn (T.append "25th percentile: " (tshow first))
  TIO.putStrLn (T.append "50th percentile: " (tshow median))
  TIO.putStrLn (T.append "75th percentile: " (tshow third))
  TIO.putStrLn (T.append "Maximum:         " (tshow maxSample))

dropLastWord :: Text -> Text
dropLastWord s = T.dropWhileEnd (not . C.isSpace) (T.stripEnd s)

parseLine :: Text -> (Text, Double)
parseLine s = (T.strip header, (read . T.unpack) measurement)
  where header = dropLastWord s
        measurement = last $ T.words s

main :: IO ()
main = do
  ls <- T.lines <$> TIO.getContents
  let parsedLines = map parseLine ls
  let groupedLines = L.groupBy ((==) `F.on` fst) parsedLines
  let headersAndStats = map headerAndAdditionalStatistics groupedLines
  sequence_ (L.intersperse (putStrLn "") (map printHeaderAndStats headersAndStats))
