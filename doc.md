- extension:
  - should not kill the server if there are multiple users of it
  - it should re-use the server if it is active
  - consider per-extension server instance
  
- implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)

- research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)

- Headers -> err:failed to parse haskell-gtd/app/server/Main.hs: Parse error: :<|> @ SrcLoc "haskell-gtd/app/server/Main.hs" 25 77
- "workspacePath is not parent of docPath, this case is broken right now" -- priority 2
- qualified imports
- operator exports
- go to module definition in import -- priority 2
- cross package re-exports -- priority 1
  - re-write `definition` to use this instead of custom functions
- when resolution fails, show the module for the requested word, if found;
  - consider caching the error(s) on per-module basis - like what lead to the error
- when parsing file fails, try parsing only imports-exports  -- priority 3
- refactoring: try creating more clear 'borders' to allow isolated testing; right now, testing is blocked by the need to perform a proper DFS
- ToJSON?

---

- https://github.com/ennocramer/floskell/issues/9 - haskell-gtd/src/GTD/Server.hs: Ambiguous infix expression @ SrcLoc "" (-1) (-1)
TODO: fix properly with imports-exports parse only and 'full' topological sort