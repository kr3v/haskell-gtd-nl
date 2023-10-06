module GTD.Server.Utils where

import qualified Data.HashMap.Strict as HMap
import Data.Maybe (fromMaybe, mapMaybe)
import GTD.Configuration (Powers (..))
import GTD.Haskell.Declaration (SourceSpan (..), UsageType (..))
import GTD.Resolution.Module.Types (UsagesInFileMap (..), UsagesPerTypeInFileMap)

x :: Powers -> SourceSpan -> UsagesPerTypeInFileMap -> [SourceSpan]
x p loc us = concat $ mapMaybe (HMap.lookup loc . _m) (x1 p us)

x1 :: Powers -> UsagesPerTypeInFileMap -> [UsagesInFileMap]
x1 p us =
  [ if _goToReferences_shouldIncludeDeclarations p then fromMaybe mempty (UDeclaration `HMap.lookup` us) else mempty,
    if _goToReferences_shouldIncludeExports p then fromMaybe mempty (Export `HMap.lookup` us) else mempty,
    if _goToReferences_shouldIncludeImports p then fromMaybe mempty (Import `HMap.lookup` us) else mempty,
    fromMaybe mempty (Regular `HMap.lookup` us)
  ]