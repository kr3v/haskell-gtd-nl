{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Utils.OS.Memory where

import Control.Applicative (liftA3)
import Control.Exception.Lifted (catch)
import Data.Char (isNumber, isSpace)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import System.Info (os)
import System.Process (readProcess)
import Text.Read (readMaybe)

parseLinuxProcMeminfo :: [String] -> Map.Map String Int
parseLinuxProcMeminfo ls = do
  let line [name, val, _] = Just (name, read val :: Int)
      line [name, val] = Just (name, read val :: Int)
      line _ = Nothing
  Map.fromList $ mapMaybe (line . words) ls

parseMacOSVmStat :: [String] -> (Int, Map.Map String Int)
parseMacOSVmStat (pageSizeLine : values) = do
  let pageSize :: Int = case words pageSizeLine of
        ["Mach", "Virtual", "Memory", "Statistics:", "(page", "size", "of", x, "bytes)"] -> fromMaybe 4096 $ readMaybe x
        _ -> 4096
  let parseLine :: String -> (String, Int)
      parseLine line =
        let (name, val) = break (== ':') line
         in (name, fromMaybe 0 $ readMaybe $ takeWhile isNumber $ dropWhile (\c -> c == ':' || isSpace c || c == '-') val)
      values' = map parseLine values
  (pageSize, Map.fromList values')
parseMacOSVmStat _ = (0, Map.empty)

procMeminfoTotal :: String
procMeminfoTotal = "MemTotal:"

procMeminfoFreeMem :: String
procMeminfoFreeMem = "MemFree:"

procMeminfoBuffers :: String
procMeminfoBuffers = "Buffers:"

procMeminfoCached :: String
procMeminfoCached = "Cached:"

vmStatPagesFree :: String
vmStatPagesFree = "Pages free"

vmStatPagesInactive :: String
vmStatPagesInactive = "Pages inactive"

vmStatPagesSpeculative :: String
vmStatPagesSpeculative = "Pages speculative"

defaultTotalMemory :: Int
defaultTotalMemory = 16 * 1024 * 1024 * 1024

defaultAvailableMemory :: Int
defaultAvailableMemory = 4 * 1024 * 1024 * 1024

macOsTotalMemory :: IO Int
macOsTotalMemory = impl `catch` (\(_ :: IOError) -> return defaultTotalMemory)
  where
    impl = do
      x <- readProcess "sysctl" ["-n", "hw.memsize"] ""
      return $ fromMaybe defaultTotalMemory $ readMaybe x

macOsAvailableMemory :: IO Int
macOsAvailableMemory = impl `catch` (\(_ :: IOError) -> return defaultAvailableMemory)
  where
    impl = do
      x <- readProcess "vm_stat" [] ""
      let (pageSize, values) = parseMacOSVmStat $ lines x
      let pagesFree = Map.lookup vmStatPagesFree values
      let pagesInactive = Map.lookup vmStatPagesInactive values
      let pagesSpeculative = Map.lookup vmStatPagesSpeculative values
      let pagesFreeT = liftA3 (\x y z -> x + y + z) pagesFree pagesInactive pagesSpeculative
      return $ maybe defaultAvailableMemory (pageSize *) pagesFreeT

linuxTotalMemory :: IO Int
linuxTotalMemory = impl `catch` (\(_ :: IOError) -> return defaultTotalMemory)
  where
    impl = do
      c <- readFile "/proc/meminfo"
      let m = parseLinuxProcMeminfo $ lines c
      return $ maybe defaultTotalMemory (1024 *) $ Map.lookup procMeminfoTotal m

linuxAvailableMemory :: IO Int
linuxAvailableMemory = impl `catch` (\(_ :: IOError) -> return defaultTotalMemory)
  where
    impl = do
      c <- readFile "/proc/meminfo"
      let m = parseLinuxProcMeminfo $ lines c
          free = Map.lookup procMeminfoFreeMem m
          buffers = Map.lookup procMeminfoBuffers m
          cached = Map.lookup procMeminfoCached m
      let freeT = liftA3 (\x y z -> x + y + z) free buffers cached
      return $ maybe defaultAvailableMemory (1024 *) freeT

totalMemory :: IO Int
totalMemory
  | os == "darwin" = macOsTotalMemory
  | os == "linux" = linuxTotalMemory
  | otherwise = return defaultTotalMemory

availableMemory :: IO Int
availableMemory
  | os == "darwin" = macOsAvailableMemory
  | os == "linux" = linuxAvailableMemory
  | otherwise = return defaultAvailableMemory