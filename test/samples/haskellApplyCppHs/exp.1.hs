#line 1 "./test/samples/haskellApplyCppHs/in.1.hs"
{-# LANGUAGE CPP #-}

-- Import window managed backend specific modules.
-- We need to use #ifdef here because if the backend library hasn't been installed
-- then we won't be able to build it, so it can't be in the import list.
module Graphics.Gloss.Internals.Interface.Backend
        ( module Graphics.Gloss.Internals.Interface.Backend.Types






        , defaultBackendState)
where

import Graphics.Gloss.Internals.Interface.Backend.Types















defaultBackendState = initBackendState
