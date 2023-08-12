- features:
  - when resolution fails, show the module for the requested word, if found;
    - consider caching the error(s) on per-module basis - like what lead to the error
  - apply cpphs via command + allow specify 'defined' stuff
  - go to symbol command (local hoogle)
  - add 'usage cases' (reverse go to definition)

- performance:
  - when file is saved, the cache is dropped for the directory; proposal: drop cache only if there's something new (like exports, imports, ...), but beware of file offsets changing

- not-yet-supported:
  - 'good':
    - Cabal options in general
  - 'bad':

- nice-to-have:
  - when parsing file fails, try parsing only imports-exports
  - show parsing status
  - `cabal get` fetches multiple versions of the same package, yet, as far as I understand, only of them is actually used during the build
  - research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)
  - 'debug' vs 'info' level configurable via vs code (hence cli args)