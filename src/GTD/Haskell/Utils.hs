module GTD.Haskell.Utils where

import qualified Data.Map as Map
import GTD.Haskell.Declaration (Declaration (..), Identifier)

asDeclsMap :: [Declaration] -> Map.Map Identifier Declaration
asDeclsMap ds = Map.fromList $ (\d -> (_declName d, d)) <$> ds
