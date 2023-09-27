- features:
  - when resolution fails, show the module for the requested word, if found;
    - consider caching the error(s) on per-module basis - like what lead to the error
  - go to symbol command (local hoogle)
  - add 'usage cases' (reverse go to definition)
  - add 'instances' list
  - goto for #include

- performance:
  - when file is saved, the cache is dropped for the directory;
    proposal: drop cache only if there's something new (like exports, imports, ...), but beware of file offsets changing

- not-yet-supported:
  - 'good':
  - 'bad':

    | - `cabal get` fetches multiple versions of the same package, yet, as far as I understand, only of them is actually used during the build
    | - research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)
		| - front-end:
		|   - if there's no `.cabal` file in the workspace, avoid using the server
		| - back-end:
		|   - consider caching last known 'good' version of file local declarations - probably a bad idea in case lines changed; however, it can be verified in runtime
    |   - if there's no resolution cache, try building it dynamically?
    |   - Cabal support:
    |     - 'global' Cabal language directives
		| - undecided:
		|   - consider supporting projects where:
		|     - there's no `.cabal` file => base only?
		|     - there's a `.cabal` file, but it's not in the root directory
		|       `./src/A/B/C/D.hs` -> check for `.cabal` in `.`, `.src`, ...; use the first that has `A.B.C.D` resolvable through `src-dirs`
  	| - both:
  	|   - follow lsp instead of custom protocol
x   |   - apply cpphs via command
    |     allow specify 'defined' stuff

cpu/memory profiling + benchmark
Consider adding documentation.

`build-depends: plutus-core:{plutus-core, plutus-ir}  ^>=1.12` -- presumably fixed, need tests.

Add a 'initialize everything' command.
Add an 'immutable' mode, where no cache gets reset.
Add support for Cabal-provided `version` and `getDataFileName` (at least to a virtual file)

`cabal get` - 1 process per many packages, unless ExitFailure (fallback to 1-1)

###
```haskell
module Text.Pretty
    ( module Export
    ) where
import Prettyprinter as Export
```

###
```haskell
instance uni1 ~ uni2 => PLC.AsNormCheckError (CompileError uni1 fun a) PLC.TyName PLC.Name uni2 fun a where
  _NormCheckError = _NoContext . _PLCError . PLC._NormCheckError

instance uni ~ DefaultUni => ToBuiltinMeaning uni NopFun where
  type CostingPart uni NopFun = NopCostModel

  data BuiltinSemanticsVariant NopFun = NopFunSemanticsVariant1

AsFreeVariableError

import Control.Lens hiding (Index, Level, index, ix)
--                          ^^^^^  ^^^^^  ^^^^^  ^^
--                          these terms are resolvable (why?)
```
###
Executables resolution should involve looking for the main file first.

In case there's no explicit type signature, the extension should still detect the 'implementations'.

###
```haskell
newtype Logger impl = LoggerTracing {unLoggerTracing :: forall a m. (ToEngineLog a impl, Tracing.MonadTraceContext m, MonadIO m) => a -> m ()}

-- | This is kept for compatibility with the old interface, which didn't
-- require a 'MonadTraceContext' environment
pattern Logger :: forall impl. (forall a m. (ToEngineLog a impl, MonadIO m) => a -> m ()) -> Logger impl
pattern Logger {unLogger} <- (newToOrig -> unLogger)
  where
    Logger f = LoggerTracing f
```

###
The extension starts working when a definition is requested from the VS Code.

The extension uses Cabal (`cabal.project` files and etc) to figure out what `*.cabal` files are present in a directory.

Once all the `*.cabal` files are parsed (and cached), the extension figures out all the Cabal 'entities' (library, executable, etc) that can 'own' the file (through `hs-source-dirs`) where the definition was initially requested.

For the matching 'entities' and all local libraries used by them, the extension clones all the Cabal dependencies into the `repos` directory at the extension root via `cabal get`.

At this point, the extension server forks to a separate process (to let the main process memory usage stay low).

The forked process parses all the entities and libraries (both local and forked) which are needed to resolve the definition.
The results are cached.

Once the forked process finishes its computation, the server fetches the 'resolution' cache from disk and returns the requested definition, if any.
