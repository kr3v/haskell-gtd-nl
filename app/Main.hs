{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens (use)
import Control.Monad (forM_)
import Control.Monad.State (StateT (..), execStateT)
import qualified Data.Map.Strict as Map
import Distribution.PackageDescription (PackageDescription)
import GTD.Cabal (CabalLibSrcDir, cabalDeps, cabalFetchDependencies, cabalRead)
import GTD.Haskell (ContextCabalPackage (..), dependencies, parseModule, parsePackage, parsePackages)
import System.FilePath ((</>))
import Text.Printf (printf)

main :: IO ()
main = do
  pkg <- cabalRead "./haskell-gtd.cabal"
  deps <- cabalDeps pkg
  let ctx = ContextCabalPackage {_modules = Map.empty, _dependencies = deps}
  c0 <- execStateT parsePackages ctx
  (d, c) <- runStateT (parseModule True "./src/GTD/Haskell.hs") c0
  printf "\n\n\n\n"
  -- print c
  -- print d