- features:
  - when resolution fails, show the module for the requested word, if found;
    - consider caching the error(s) on per-module basis - like what lead to the error
x - apply cpphs via command
    - allow specify 'defined' stuff
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

- nice-to-have:
  - `cabal get` fetches multiple versions of the same package, yet, as far as I understand, only of them is actually used during the build
  - research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)

		| - front-end:
		|   - if there's no `.cabal` file in the workspace, avoid using the server
		| - back-end:
8		|   - consider caching last known 'good' version of file local declarations - probably a bad idea in case lines changed; however, it can be verified in runtime
9   |   - if there's no resolution cache, try building it dynamically?
    |   - Cabal support:
    |     - 'global' Cabal language directives
		| - undecided:
		|   - consider supporting projects where:
		|     - there's no `.cabal` file => base only?
		|     - there's a `.cabal` file, but it's not in the root directory
		|       `./src/A/B/C/D.hs` -> check for `.cabal` in `.`, `.src`, ...; use the first that has `A.B.C.D` resolvable through `src-dirs`
  	| - both:
  	|   - follow lsp instead of custom protocol

cpu/memory profiling + benchmark
Update README.md (plus the recording).
Consider adding documentation.
Try using the extension on a real project (HLS, Servant, Cabal, ..., maybe something from IOHK).

`build-depends: plutus-core:{plutus-core, plutus-ir}  ^>=1.12` -- presumably fixed, need tests.

Add a 'initialize everything' command.
Add an 'immutable' mode, where no cache gets reset.
Add support for Cabal-provided `version` and `getDataFileName` (at least to a virtual file)

```haskell
module Text.Pretty
    ( module Export
    ) where
import Prettyprinter as Export
```

```haskell
instance uni1 ~ uni2 => PLC.AsNormCheckError (CompileError uni1 fun a) PLC.TyName PLC.Name uni2 fun a where
    _NormCheckError = _NoContext . _PLCError . PLC._NormCheckError

AsFreeVariableError

import Control.Lens hiding (Index, Level, index, ix)
--                          ^^^^^  ^^^^^  ^^^^^  ^^
--                          these terms are resolvable (why?)
```

Executables resolution should involve looking for the main file first.

---

### HLS
Two concerns:
1. HLS does not support `base` package, so I wasn't sure that navigation in it would work.
2. I was not sure if whatever in-HLS implementation I would come up with would be accepted by the HLS team, so I decided to go with my own extension just to be sure that I would be able using it in the future.

The application is standalone (it does not depend on HLS in any way) 
---

```shell
npm install
npm install webpack webpack-cli
# npm install -g webpack webpack-cli
```

```shell @ ubuntu
apt install libgmp3-dev zlib1g-dev
```