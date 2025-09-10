{-# LANGUAGE OverloadedStrings #-}

import Formatting ((%))
import qualified Formatting as Fmt

import qualified Statistics.Quantile as S
import qualified Statistics.Sample   as SS

import qualified Data.Char           as C
import qualified Data.Function       as F
import qualified Data.List           as L
import           Data.String         (IsString)
import qualified Data.String         as Str
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import qualified Control.Monad       as M

fmt :: Fmt.Format r (Double -> r)
fmt = Fmt.fixed 5

quantile :: Int -> Vector Double -> Double
quantile q xs = S.weightedAvg q 4 xs

headerAndStats :: Text -> Vector Double -> IO ()
headerAndStats header samples = do
  M.unless (T.strip header == "") (TIO.putStrLn header)
  Fmt.fprint ("Samples: " % Fmt.int % "\n") (V.length samples)
  Fmt.fprint ("Average: " % fmt % "\n") (SS.mean samples)
  Fmt.fprint ("Minimum: " % Fmt.float % "\n") (V.minimum samples)
  Fmt.fprint ("25th percentile: " % fmt % "\n") (quantile 1 samples)
  Fmt.fprint ("50th percentile: " % fmt % "\n") (quantile 2 samples)
  Fmt.fprint ("75th percentile: " % fmt % "\n") (quantile 3 samples)
  Fmt.fprint ("90th percentile: " % fmt % "\n") (S.weightedAvg 9 10 samples)
  Fmt.fprint ("99th percentile: " % fmt % "\n") (S.weightedAvg 99 100 samples)
  Fmt.fprint ("99.9th percentile: " % fmt % "\n") (S.weightedAvg 999 1000 samples)
  Fmt.fprint ("Maximum: " % Fmt.float % "\n") (V.maximum samples)

stringOfList :: (IsString s) => [s] -> s
stringOfList = foldr (\hd _ -> hd) (Str.fromString "")

headerAndSamples :: [(Text, Double)] -> (Text, Vector Double)
headerAndSamples tds =
  (stringOfList headers, V.fromList samples)
  where (headers, samples) = unzip tds

dropLastWord :: Text -> Text
dropLastWord = (T.dropWhileEnd (not . C.isSpace)) . T.stripEnd

parseLine :: Text -> (Text, Double)
parseLine l = (T.strip header, (read . T.unpack) measurement)
  where header = dropLastWord l
        measurement = last $ T.words l

main :: IO ()
main = do
  ls <- T.lines <$> TIO.getContents
  let parsedLines = map parseLine ls
  let groupedLines = L.groupBy ((==) `F.on` fst) parsedLines
  let headersAndSamples = map headerAndSamples groupedLines
  sequence_ (L.intersperse (putStrLn "") (map (uncurry headerAndStats) headersAndSamples))
