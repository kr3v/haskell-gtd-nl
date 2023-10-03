- features:
  - when resolution fails, show the module for the requested word, if found;
    - consider caching the error(s) on per-module basis - like what lead to the error
  - go to symbol command (local hoogle)
  - add 'usage cases' (reverse go to definition)
    - drop the cache on save
  - add 'instances' list
  - goto for #include
  - 'switch from `let` to `<-`' refactoring - this is maybe easy, just ask the user to select the whole "statement", parse it with GHC and transform it
  - 'extract variable' - potentially hard, because it is not exactly obvious where to put the extracted variable
      - a trivial "take an expression, declare it with `let` on the 'previous' line" might because there's no `do` block; also, there's a potential problem with the indentation
  - 'smart' operator flip (only known operators?)
  - extension -> server tests
  - foldr and function aggregation: i want 'first' functions to be applied as early as possible

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

Add an 'initialize everything' command.
Add an 'immutable' mode, where no cache gets reset.
Add support for Cabal-provided `version` and `getDataFileName` (at least to a virtual file)

`cabal get` - 1 process per many packages, unless ExitFailure (fallback to 1-1)

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

---

```tsv
n			a				copied_gib	cpu_s		elapsed_s	gc_cpu_s	gc_elapsed_s	max_live_gib	max_mem_in_use_gib	mutator_cpu_s	mutator_elapsed_s
-N1		-A1M		35.20				38.38		46.26			17.92			17.98					1.03					2.73								20.45					28.22
-N1		-A4M		25.75				34.58		41.44			13.56			13.59					1.01					2.63								21.02					27.88
-N1		-A32M		16.63				37.43		44.54			11.42			11.45					1.00					2.41								25.98					33.02
-N1		-A128M	12.10				38.87		46.03			10.08			10.10					0.99					2.31								28.78					35.94
-N1		-A512M	8.15				37.57		44.34			7.39			7.40					0.98					2.71								30.15					36.90
-N8		-A1M		34.66				55.14		29.51			29.46			10.90					1.03					2.75								25.70					18.62
-N8		-A4M		24.20				42.51		25.27			18.92			7.94					0.99					2.55								23.59					17.32
-N8		-A32M		10.26				47.32		25.62			15.09			2.94					1.03					2.43								32.29					22.68
-N8		-A128M	6.29				43.16		24.69			10.17			1.74					0.98					3.19								32.98					22.95
-N8		-A512M	3.53				38.65		23.52			5.96			0.88					0.84					6.47								32.68					22.63
-N24	-A1M		35.33				178.85	56.59			109.27		20.89					1.03					2.76								69.78					35.69
-N24	-A4M		24.36				94.14		37.37			56.09			12.47					1.02					2.70								38.58					24.90
-N24	-A32M		7.48				62.92		26.61			29.61			2.12					0.89					2.74								33.32					24.49
-N24	-A128M	4.31				51.63		23.64			18.89			1.13					0.82					5.01								32.74					22.49
-N24	-A512M	1.79				41.13		22.45			9.20			0.52					0.49					14.39								31.93					21.93
```

-N8 and -A32M is the best? Kind of low `cpu_s`, not so much worse `copied_gib` than in `-A128M`, great `max_mem_in_use_gib`, ~110% of the best elapsed time.
-N8 and -A128M is also reasonable, given that CPU parameters are better for it and `max_mem_in_use_gib` is only slightly worse (higher).
-N1 is the great with -A4M, but `copied_gib` is ???
Also, As <= -A32M have the same `max_mem_in_use_gib`, maybe there's no reason to use -A4M (especially with N > 1).
