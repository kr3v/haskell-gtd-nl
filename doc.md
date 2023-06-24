1. extension:
  - should not kill the server if there are multiple users of it
  - it should re-use the server if it is active
  - consider per-extension server instance
  
2. implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)
  - module re-export

3. research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)

4. https://github.com/ennocramer/floskell/issues/9 - haskell-gtd/src/GTD/Server.hs: Ambiguous infix expression @ SrcLoc "" (-1) (-1)

5. Headers -> err:failed to parse haskell-gtd/app/server/Main.hs: Parse error: :<|> @ SrcLoc "haskell-gtd/app/server/Main.hs" 25 77

6. "workspacePath is not parent of docPath, this case is broken right now"

7. qualified imports

8. go to module definition in import
