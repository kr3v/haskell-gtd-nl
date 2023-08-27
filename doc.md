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
		|   - show parsing status: server/package executable should occasionally print their status to a file in the local/share dir
8		|   - consider caching last known 'good' version of file local declarations
9   |   - if there's no resolution cache, try building it dynamically?
    |   - dropcache tests
    |   - drop `Context` usage in `GTD.Cabal` package by replacing it with direct access
    |   - Cabal support:
    |     - locally resolved libraries should override `cabal get`
    |     - 'global' Cabal language directives
		| - undecided:
		|   - consider supporting projects where:
		|     - there's no `.cabal` file (base only?)
		|     - there's a `.cabal` file, but it's not in the root directcory
		|       `./src/A/B/C/D.hs` -> check for `.cabal` in `.`, `.src`, ...; use the first that has `A.B.C.D` resolvable through `src-dirs`
  	| - both:
  	|   - follow lsp instead of custom protocol

`concat`?
`*.hsc` files
