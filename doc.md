- extension:
  - should not kill the server if there are multiple users of it
  - it should re-use the server if it is active
  - consider per-extension server instance
  
- implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)

- research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)

- Headers -> err:failed to parse haskell-gtd/app/server/Main.hs: Parse error: :<|> @ SrcLoc "haskell-gtd/app/server/Main.hs" 25 77

- "workspacePath is not parent of docPath, this case is broken right now"

- qualified imports

- operator exports 

- go to module definition in import

- cross package re-exports

- `data` definitions - just fetch Decl -> DataDecl -> DeclHead l -> (DHead, DHApp -> walk until DHead, if any)
  - fields: QualConDecl -> ConDecl -> Name

---

- https://github.com/ennocramer/floskell/issues/9 - haskell-gtd/src/GTD/Server.hs: Ambiguous infix expression @ SrcLoc "" (-1) (-1)
TODO: fix properly with imports-exports parse only and 'full' topological sort