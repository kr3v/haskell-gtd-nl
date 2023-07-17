- extension:
  - should not kill the server if there are multiple users of it
  - it should re-use the server if it is active
  - consider per-extension server instance
  
- implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)

- research Haskell formatters to avoid lines longer than 80/120 characters (wrap the imports, wrap the exports, wrap function declarations, ...)

- Headers -> err:failed to parse haskell-gtd/app/server/Main.hs: Parse error: :<|> @ SrcLoc "haskell-gtd/app/server/Main.hs" 25 77
- qualified imports
- operator exports
- go to module definition in import
- when resolution fails, show the module for the requested word, if found;
  - consider caching the error(s) on per-module basis - like what lead to the error
- when parsing file fails, try parsing only imports-exports
- refactoring: try creating more clear 'borders' to allow isolated testing; right now, testing is blocked by the need to perform a proper DFS
- ToJSON?

- lru: either fork the library or find something with the following: in case entries were not touched for a long time (15 min?), just silently drop them
- parallelize `cabal read`s if necessary
- parallelize packages processing
- parallelize modules processing
- research haskell gc behaviour (haskell allocates more than it uses and it is even worse compared to Golang GOGC)
- Prelude support
-
```
BangPatterns language extension is not enabled
BlockArguments language extension is not enabled
ConstraintKinds language extension is not enabled
DataKinds language extension is not enabled
DefaultSignatures language extension is not enabled
ExistentialQuantification language extension is not enabled
ExplicitForAll language extension is not enabled
ExplicitNamespaces language extension is not enabled
FunctionalDependencies language extension is not enabled
GADTs language extension is not enabled
KindSignatures language extension is not enabled
MultiParamTypeClasses language extension is not enabled
PackageImports language extension is not enabled (`import "crypto-api" Crypto.Random`)
ScopedTypeVariables language extension is not enabled
StandaloneDeriving language extension is not enabled
TemplateHaskell language extension is not enabled
TypeFamilies language extension is not enabled
TypeOperators language extension is not enabled
```
- `cabal get` fetches multiple versions of the same package, yet, as far as I understand, only of them is actually used during the build
- immutable via vs code on `.repos`

---

- https://github.com/ennocramer/floskell/issues/9 - haskell-gtd/src/GTD/Server.hs: Ambiguous infix expression @ SrcLoc "" (-1) (-1)
TODO: fix properly with imports-exports parse only and 'full' topological sort