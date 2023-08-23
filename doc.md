- features:
  - when resolution fails, show the module for the requested word, if found;
    - consider caching the error(s) on per-module basis - like what lead to the error
  - apply cpphs via command + allow specify 'defined' stuff
  - go to symbol command (local hoogle)
  - add 'usage cases' (reverse go to definition)
  - add 'instances' list

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
3x	|   - if `hls` is active, disable go-to-definition in local non-.repos repo by default (3 states - disabled, disabled_if_hls, enabled)
1x	|   - add support for multiple workspace directories
2x	|   - try watching for `port=\d+` in the server output instead of `sleep`
		|   - if there's no `.cabal` file in the workspace, avoid using the server
		| - back-end:
4x	|   - 'dynamic' package memory usage: if available RAM ... => ... : >=8 GiB => -N -A128M, <8 GiB => -N -A64M, <4 GiB => -A16M, <2 GiB => no options
5x	|   - server logging: 'debug' vs 'info' level configurable
		|   - show parsing status: server/package executable should occasionally print their status to a file in the local/share dir
6x	|   - if parsing a file fails, try parsing only imports-exports
8		|     - consider caching last known 'good' version of file local declarations
9   |   - if there's no resolution cache, try building it dynamically?
    |   - Cabal support:
    |     - `cabal.project`
x?  |     - `common` in `*.cabal`
x?  |     - `haskell-language-server.cabal`: `build-depends` without version predicates
		| - undecided:
		|   - consider supporting projects where:
		|     - there's no `.cabal` file (base only?)
		|     - there's a `.cabal` file, but it's not in the root directory
		|       `./src/A/B/C/D.hs` -> check for `.cabal` in `.`, `.src`, ...; use the first that has `A.B.C.D` resolvable through `src-dirs`
  	| - both:
  	|   - follow lsp instead of custom protocol
7x	|   - support `~/.cabal/bin/...` and PATH for `haskell-gtd-server` and `haskell-gtd-package`

---

DistDirLayout -> err=cannot find a cabal 'item' with source directory that owns given file (data={"err":"cannot find a cabal 'item' with source directory that owns given file","srcSpan":null})
