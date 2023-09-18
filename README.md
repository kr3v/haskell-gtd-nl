# Haskell 'go to (non-local)* definitions' extension for VS Code

This repository contains a VS Code extension and a server that implements a 'go to definition' command for Haskell code.

It relies on `ghc-lib-parser` to parse Haskell code. Supports only `*.cabal`-based projects.

The extension server stores its info at `~/.local/share/haskell-gtd-nl/` directory. Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.
`repos` directory is added to a working directory root as a `.repos` symlink (to prevent https://github.com/haskell/vscode-haskell/issues/480).

General notes:
1. The extension works only in a project with a `*.cabal` file at its root.
2. The first attempt to perform a `go to definition` action might take some time, yet consequent attempts in the same VS Code session should take much less time.

## How to install
1. Install the VS Code extension (through either `*.vsix` or marketplace).
2. Clone this repository. Execute `cabal install` in its root directory. On Ubuntu22, I had to install `libgmp3-dev`, `zlib1g-dev` and `build-essential` packages for `cabal install` to succeed.
```shell
git clone https://github.com/kr3v/haskell-gtd-nl
cd haskell-gtd-nl
cabal install
```
3. The extension should activate when a Haskell file gets opened.

## Overview
The extension starts working when a definition is requested from the VS Code (`extension/src/extension.ts` - `POST /definition` @ `app/server/Main.hs:definition` -> `src/GTD/Server.hs:definition`).

The extension uses Cabal (`cabal.project` files and etc) to figure out what `*.cabal` files are present in a directory (`src/GTD/Cabal/FindAt.hs:findAt'cabalProject`).

Once all the `*.cabal` files are parsed (and cached) (`src/GTD/Cabal/Parse.hs:parse`), the extension figures out all the Cabal 'entities' (library, executable, etc) that can 'own' the file (through `hs-source-dirs`) where the definition was initially requested (`src/GTD/Server.hs:cabalPackage`).

For the matching 'entities' and all local libraries used by them, the extension clones all the Cabal dependencies into the `repos` directory at the extension root via `cabal get` (`src/GTD/Cabal/Dependencies.hs:full`, `src/GTD/Cabal/Get.hs:get`).

At this point, the extension server forks to a separate process (to let the main process memory usage stay low) (`src/GTD/Server.hs:package'resolution'withDependencies'forked` -> `app/package/Main.hs`).
The forked process parses all the entities and libraries (both local and forked) which are needed to resolve the definition (`src/GTD/Server.hs:(package'resolution'withDependencies'concurrently -> package'resolution -> modules -> )` -> `` -> ).
The results are cached.

Once the forked process finishes its computation, 
