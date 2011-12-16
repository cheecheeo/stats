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

newtype Stat = Stat (Text, Vector Double -> Double)

numSamples, average, minimumStat, first, median, third, maximumStat :: Stat
numSamples = Stat ("Samples: ", fromIntegral . V.length)
average = Stat ("Average: ", SS.mean)
minimumStat = Stat ("Minimum: ", V.minimum)
first = Stat ("25th percentile: ", quantile 1)
median = Stat ("50th percentile: ", quantile 2)
third = Stat ("75th percentile: ", quantile 3)
maximumStat = Stat ("Maximum: ", V.maximum)

quantile :: Int -> Vector Double -> Double
quantile q xs = S.weightedAvg q 4 xs

allStats :: [Stat]
allStats = [numSamples, average, minimumStat, first, median, third, maximumStat]

headerAndStats :: [(Text, Double)] -> IO ()
headerAndStats hss = do
  M.unless (T.strip header == "") (TIO.putStrLn header)
  mapM_ (\(Stat (text, stat)) -> TIO.putStrLn (T.append text ((tshow . stat . V.fromList) samples))) allStats
  where (headers, samples) = unzip hss
        header = foldr (\hd _ -> hd) "" headers

tshow :: (Show a) => a -> Text
tshow = T.pack . show

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
  sequence_ (L.intersperse (putStrLn "") (map headerAndStats groupedLines))
