import qualified Data.Vector.Unboxed as V
import Criterion.Report (Report(..))
import qualified Criterion.Report as CR
import qualified Criterion.Analysis as CA
import Statistics.Types (Sample)
import qualified Data.ByteString.Lazy as B

testSample :: Sample
testSample = V.fromList $ [41.5] ++ (replicate 1000 42)

samples2Reports :: [Sample] -> IO [Report]
samples2Reports ss = do
  -- http://hackage.haskell.org/packages/archive/criterion/0.6.0.0/doc/html/src/Criterion-Config.html#defaultConfig
  analysiss <- mapM (\sample -> CA.analyseSample 0.95 sample (100 * 1000)) ss
  -- TODO [Sample] should be [(Text, Sample)] where Text is the "name" of the benchmark
  return $ zipWith3 (\number sample analysis -> Report number "foo" sample analysis (CA.classifyOutliers sample)) [0..] ss analysiss

sample2Report :: Sample -> IO Report
sample2Report sample = do
  -- http://hackage.haskell.org/packages/archive/criterion/0.6.0.0/doc/html/src/Criterion-Config.html#defaultConfig
  analysis <- CA.analyseSample 0.95 sample (100 * 1000)
  return $ Report 0 "foo" sample analysis (CA.classifyOutliers sample)

main :: IO ()
main = do
  report <- sample2Report testSample
  --print =<< report
  outputMe <- CR.formatReport [report] =<< CR.loadTemplate ["."] "report.tpl"
  B.putStr outputMe
