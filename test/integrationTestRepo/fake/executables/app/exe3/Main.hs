module Main where

import Control.Exception (try)
import Control.Lens (makeLenses, over, view, (%=), (&), (.~), (^.))
import Control.Monad.State (MonadState (..), State, evalState, execState, modify, runState)
import Data.Data (Proxy (..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Lib1
import System.Random (mkStdGen)
import Text.Printf (printf)

import qualified Control.Lens.Getter as Lens (Getter, Contravariant, Const, to)
import qualified Control.Lens.Setter as Lens


exe3 :: Int
exe3 = 1

main :: IO ()
main = putStrLn "Hello, Haskell!"
