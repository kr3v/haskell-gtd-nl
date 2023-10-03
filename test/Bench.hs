{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Criterion.Main (bench, perRunEnvWithCleanup, runMode)
import Criterion.Main.Options (MatchType (..), Mode (..), defaultConfig)
import Criterion.Types (Config (..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Time (ZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector.Unboxed as V
import GHC.RTS.Flags (getRTSFlags)
import GHC.Stats (RTSStats (..), getRTSStats)
import GTD.Cabal.Types (Designation (..), DesignationType (..))
import GTD.Configuration (Args (..), GTDConfiguration (..), Powers, defaultArgs, powersP, prepareConstants)
import qualified GTD.ParserExe as ParserExe
import GTD.Utils (concatForM)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, metavar, option, optional, showDefault, strOption, subparser, value, (<**>))
import Options.Applicative.Builder (command)
import Statistics.Sample (mean, stdDev)
import System.Directory (createDirectoryIfMissing, listDirectory, makeAbsolute, removeDirectoryRecursive)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hPrint, hSetBuffering, stderr, stdout, withFile)
import Text.Read (readMaybe)

data BArgs
  = BArgs
      { _criterionReport :: FilePath,
        _ownReport :: FilePath,
        _repo :: FilePath,
        _des :: Designation,
        _powers :: Powers
      }
  | RArgs
      { _dir :: FilePath
      }
  deriving (Show, Read)

bargsP :: Parser BArgs
bargsP =
  BArgs
    <$> strOption (long "criterion-report-path" <> metavar "FILE" <> help "write JSON output to FILE" <> showDefault <> value "report.json")
    <*> strOption (long "report-path" <> metavar "FILE" <> help "repo where to run the benchmark" <> showDefault <> value "report.txt")
    <*> strOption (long "repo-path" <> metavar "FILE" <> help "repo where to run the benchmark" <> showDefault <> value "test/integrationTestRepo/fake")
    <*> ( Designation
            <$> optional (strOption (long "des-name" <> metavar "NAME" <> help "designation name" <> showDefault <> value "exe3"))
            <*> option auto (long "des-type" <> metavar "TYPE" <> help "designation type" <> showDefault <> value Executable)
        )
    <*> powersP

rargsP :: Parser BArgs
rargsP =
  RArgs
    <$> strOption (long "dir" <> metavar "FILE" <> help "root of the project" <> showDefault)

opts :: Parser a -> ParserInfo a
opts a =
  info
    (a <**> helper)
    fullDesc

argsP :: Parser BArgs
argsP = do
  let commands =
        [ command "bench" $ opts bargsP,
          command "report" $ opts rargsP
        ]
  subparser (mconcat commands)

selectors :: [(String, RTSStats -> Double)]
selectors =
  [ ("gcs", fromIntegral . gcs),
    ("major_gcs", fromIntegral . major_gcs),
    ("allocated_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . allocated_bytes),
    ("max_live_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . max_live_bytes),
    ("max_large_objects_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . max_large_objects_bytes),
    ("max_compact_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . max_compact_bytes),
    ("max_slop_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . max_slop_bytes),
    ("max_mem_in_use_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . max_mem_in_use_bytes),
    ("cumulative_live_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . cumulative_live_bytes),
    ("copied_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . copied_bytes),
    ("par_copied_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . par_copied_bytes),
    ("cumulative_par_max_copied_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . cumulative_par_max_copied_bytes),
    ("cumulative_par_balanced_copied_gib", (/ (1024 * 1024 * 1024)) . fromIntegral . cumulative_par_balanced_copied_bytes),
    ("init_cpu_s", (/ 1e9) . fromIntegral . init_cpu_ns),
    ("init_elapsed_s", (/ 1e9) . fromIntegral . init_elapsed_ns),
    ("mutator_cpu_s", (/ 1e9) . fromIntegral . mutator_cpu_ns),
    ("mutator_elapsed_s", (/ 1e9) . fromIntegral . mutator_elapsed_ns),
    ("gc_cpu_s", (/ 1e9) . fromIntegral . gc_cpu_ns),
    ("gc_elapsed_s", (/ 1e9) . fromIntegral . gc_elapsed_ns),
    ("cpu_s", (/ 1e9) . fromIntegral . cpu_ns),
    ("elapsed_s", (/ 1e9) . fromIntegral . elapsed_ns),
    ("nonmoving_gc_sync_cpu_s", (/ 1e9) . fromIntegral . nonmoving_gc_sync_cpu_ns),
    ("nonmoving_gc_sync_elapsed_s", (/ 1e9) . fromIntegral . nonmoving_gc_sync_elapsed_ns),
    ("nonmoving_gc_sync_max_elapsed_s", (/ 1e9) . fromIntegral . nonmoving_gc_sync_max_elapsed_ns),
    ("nonmoving_gc_cpu_s", (/ 1e9) . fromIntegral . nonmoving_gc_cpu_ns),
    ("nonmoving_gc_elapsed_s", (/ 1e9) . fromIntegral . nonmoving_gc_elapsed_ns),
    ("nonmoving_gc_max_elapsed_s", (/ 1e9) . fromIntegral . nonmoving_gc_max_elapsed_ns)
  ]

select :: [(String, RTSStats -> Double)] -> [RTSStats] -> [(String, [Double])]
select ss vs = flip fmap ss $ \(n, f) -> (n, f <$> vs)

select0 :: [RTSStats] -> [(String, [Double])]
select0 = select selectors

-- copilot: generate me a sub function `a - b` for all the fields
sub :: RTSStats -> RTSStats -> RTSStats
sub a b =
  RTSStats
    { gcs = gcs a - gcs b,
      major_gcs = major_gcs a - major_gcs b,
      allocated_bytes = allocated_bytes a - allocated_bytes b,
      max_live_bytes = max (max_live_bytes a) (max_live_bytes b),
      max_large_objects_bytes = max (max_large_objects_bytes a) (max_large_objects_bytes b),
      max_compact_bytes = max (max_compact_bytes a) (max_compact_bytes b),
      max_slop_bytes = max (max_slop_bytes a) (max_slop_bytes b),
      max_mem_in_use_bytes = max (max_mem_in_use_bytes a) (max_mem_in_use_bytes b),
      cumulative_live_bytes = cumulative_live_bytes a - cumulative_live_bytes b,
      copied_bytes = copied_bytes a - copied_bytes b,
      par_copied_bytes = par_copied_bytes a - par_copied_bytes b,
      cumulative_par_max_copied_bytes = cumulative_par_max_copied_bytes a - cumulative_par_max_copied_bytes b,
      cumulative_par_balanced_copied_bytes = cumulative_par_balanced_copied_bytes a - cumulative_par_balanced_copied_bytes b,
      init_cpu_ns = init_cpu_ns a - init_cpu_ns b,
      init_elapsed_ns = init_elapsed_ns a - init_elapsed_ns b,
      mutator_cpu_ns = mutator_cpu_ns a - mutator_cpu_ns b,
      mutator_elapsed_ns = mutator_elapsed_ns a - mutator_elapsed_ns b,
      gc_cpu_ns = gc_cpu_ns a - gc_cpu_ns b,
      gc_elapsed_ns = gc_elapsed_ns a - gc_elapsed_ns b,
      cpu_ns = cpu_ns a - cpu_ns b,
      elapsed_ns = elapsed_ns a - elapsed_ns b,
      nonmoving_gc_sync_cpu_ns = nonmoving_gc_sync_cpu_ns a - nonmoving_gc_sync_cpu_ns b,
      nonmoving_gc_sync_elapsed_ns = nonmoving_gc_sync_elapsed_ns a - nonmoving_gc_sync_elapsed_ns b,
      nonmoving_gc_sync_max_elapsed_ns = nonmoving_gc_sync_max_elapsed_ns a - nonmoving_gc_sync_max_elapsed_ns b,
      nonmoving_gc_cpu_ns = nonmoving_gc_cpu_ns a - nonmoving_gc_cpu_ns b,
      nonmoving_gc_elapsed_ns = nonmoving_gc_elapsed_ns a - nonmoving_gc_elapsed_ns b,
      nonmoving_gc_max_elapsed_ns = nonmoving_gc_max_elapsed_ns a - nonmoving_gc_max_elapsed_ns b,
      gc = gc b
    }

withDiff :: Handle -> IO () -> IO ()
withDiff h a = do
  s1 <- getRTSStats
  a
  s2 <- getRTSStats
  hPrint h $ sub s2 s1

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering

  ca0 <- execParser $ opts argsP
  print ca0
  case ca0 of
    RArgs {_dir = d} -> do
      -- date and time when the benchmark was started
      let dt0 :: Maybe ZonedTime = iso8601ParseM $ takeFileName d
      let dt0f = formatTime defaultTimeLocale "%F %T" <$> dt0

      fs <- filter (".txt" `isSuffixOf`) <$> listDirectory d
      ls <- concatForM fs $ \f -> do
        ls <- lines <$> readFile (d <> "/" <> f)
        let dt1 :: Maybe ZonedTime = iso8601ParseM $ case splitOn "." f of
              (rtsArgs : dt : [ext]) -> dt
              _ -> "???"
        let dt1f = formatTime defaultTimeLocale "%F %T" <$> dt1

        return $ case ls of
          (rtsArgs : bargsS : rtsConf : stats) -> do
            let (bargs :: Maybe BArgs) = readMaybe bargsS
            let vs :: [(String, [Double])] = select0 $ mapMaybe readMaybe stats
            vs <&> \(n, v) -> do
              let z = V.fromList v
              JSON.encode $
                (,,,,,,,,)
                  dt0f
                  dt1f
                  rtsArgs
                  (_desName . _des <$> bargs)
                  (_desType . _des <$> bargs)
                  (_repo <$> bargs)
                  n
                  (mean z)
                  (stdDev z)
          _ -> []
      BS.writeFile (d </> "aggregated.json") $ BS.unlines ls
      BS.writeFile (takeDirectory d </> (takeFileName d ++ "_" ++ "aggregated.json")) $ BS.unlines ls
    ca@BArgs {} ->
      withFile (_ownReport ca) AppendMode $ \h -> do
        hSetBuffering h LineBuffering

        hPrint h ca

        rt <- makeAbsolute "test/root"
        a <- defaultArgs
        c <- prepareConstants (a {_root = rt})
        hPrint h c

        fs <- getRTSFlags
        hPrint h fs

        let cleanup = do
              removeDirectoryRecursive (_cache c)
              removeDirectoryRecursive (_cacheUsages c)
              createDirectoryIfMissing True (_cache c)
              createDirectoryIfMissing True (_cacheUsages c)

        let cfg = defaultConfig {jsonFile = Just $ _criterionReport ca}
        let bs =
              [ bench "parse a repo" $
                  perRunEnvWithCleanup
                    cleanup
                    return
                    (\_ -> liftIO $ withDiff h $ runNoLoggingT $ ParserExe.parserExe c (_repo ca) (_des ca))
              ]

        runMode
          (Run cfg Pattern [""])
          bs