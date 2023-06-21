# Haskell 'go to (non-local) definitions' extension
Both 'server' and 'front-end' (VS Code extension for now).

Relies on `cpphs` and `haskell-src-exts` to parse Haskell code.
Supports only `*.cabal`-based projects.

Extension server stores its info at `~/.local/share/haskell-gtd-extension-server-root` directory. Logs are not rotated (yet). Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.

Only most basic cases are supported, like:

```haskell

-- cabal-based library L
module A where -- both implicit and explicit export lists are supported

f :: Int
f = 1
```

```haskell
-- cabal-based (executable?) E in a different cabal file
-- library L should be 'obtainable' via `cabal get` command
module B where

import A (f) -- `go to definition` command should work for `f` here

g :: Int
g = f        -- and here
```

Any other cases (re-exported modules and functions, classes and data types, instances, etc) are not supported at the moment.

## How to run
1. `cabal run haskell-gtd-server`
2. `cd extension; code .`
3. Start debugging the extension via F5.

## Help
Code review and suggestions are requested.
