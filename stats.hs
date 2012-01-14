{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

-- cabal update && cabal install text statistics

import Statistics.Types (Sample)
import qualified Statistics.Quantile as S
import qualified Statistics.Sample as SS

import qualified Data.Function as F
import qualified Data.Char as C
import Data.String (IsString)
import qualified Data.String as Str
import qualified Data.List as L
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Control.Applicative ((<$>))
import qualified Control.Monad as M

import Criterion.Report (Report(..))
import qualified Criterion.Report as CR
import qualified Criterion.Analysis as CA
import qualified Data.ByteString.Lazy as B

import qualified System.Environment as Env
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import qualified System.Console.GetOpt as Opt
import qualified System.IO as IO

newtype Stat = Stat (Text, Vector Double -> Double)

numSamples, average, minimumStat, first, median, third, ninetieth, ninetyNinth, oneNine, maximumStat :: Stat
numSamples = Stat ("Samples: ", fromIntegral . V.length)
average = Stat ("Average: ", SS.mean)
minimumStat = Stat ("Minimum: ", V.minimum)
first = Stat ("25th percentile: ", quantile 1)
median = Stat ("50th percentile: ", quantile 2)
third = Stat ("75th percentile: ", quantile 3)
ninetieth = Stat ("90th percentile: ", S.weightedAvg 9 10)
ninetyNinth = Stat ("99th percentile: ", S.weightedAvg 99 100)
oneNine = Stat ("99.9th percentile: ", S.weightedAvg 999 1000)
maximumStat = Stat ("Maximum: ", V.maximum)

quantile :: Int -> Vector Double -> Double
quantile q xs = S.weightedAvg q 4 xs

allStats :: [Stat]
allStats = [numSamples, average, minimumStat, first, median, third, ninetieth, ninetyNinth, oneNine, maximumStat]

headerAndStats :: Text -> Vector Double -> IO ()
headerAndStats header samples = do
  M.unless (T.strip header == "") (TIO.putStrLn header)
  mapM_ (\(Stat (text, stat)) -> TIO.putStrLn (T.append text ((tshow . stat) samples))) allStats

stringOfList :: (IsString s) => [s] -> s
stringOfList = foldr (\hd _ -> hd) (Str.fromString "")

headerAndSamples :: [(Text, Double)] -> (Text, Vector Double)
headerAndSamples tds =
  (stringOfList headers, V.fromList samples)
  where (headers, samples) = unzip tds

tshow :: (Show a) => a -> Text
tshow = T.pack . show

dropLastWord :: Text -> Text
dropLastWord s = T.dropWhileEnd (not . C.isSpace) (T.stripEnd s)

parseLine :: Text -> (Text, Double)
parseLine s = (T.strip header, (read . T.unpack) measurement)
  where header = dropLastWord s
        measurement = last $ T.words s

-- FIXME? sample2Report :: Int -> Text -> Sample -> IO Report
samples2Reports :: [(Text, Sample)] -> IO [Report]
samples2Reports headersSamples = do
  -- http://hackage.haskell.org/packages/archive/criterion/0.6.0.0/doc/html/src/Criterion-Config.html#defaultConfig
  analysiss <- mapM (\sample -> CA.analyseSample 0.95 sample (100 * 1000)) samples
  -- TODO [Sample] should be [(Text, Sample)] where Text is the "name" of the benchmark
  return $ L.zipWith4 (\number header sample analysis -> Report number (show header) sample analysis (CA.classifyOutliers sample)) [0..] headers samples analysiss
  where (headers, samples) = unzip headersSamples

options :: [OptDescr Bool]
options =
     [ Option ['c'] ["chart"] (NoArg True) "output html charts" ]

main :: IO ()
main = do
  argv <- Env.getArgs
  case Opt.getOpt Permute options argv of
    (charts,_,[]) -> do
      ls <- T.lines <$> TIO.getContents
      let parsedLines = map parseLine ls
      let groupedLines = L.groupBy ((==) `F.on` fst) parsedLines
      let headersAndSamples = map headerAndSamples groupedLines
      case charts of
        (True : _) -> do
          reports <- samples2Reports headersAndSamples
          B.putStr =<< CR.formatReport reports =<< CR.loadTemplate ["."] "report.tpl"
        _ -> sequence_ (L.intersperse (putStrLn "") (map (uncurry headerAndStats) headersAndSamples))
    (_,_,errs) -> IO.hPutStr IO.stderr (concat errs ++ Opt.usageInfo header options)
  where header = "Usage: stats [OPTION]..."
