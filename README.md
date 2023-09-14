# Haskell 'go to (non-local)* definitions' extension for VS Code

This repository contains a VS Code extension and a server that implements a 'go to definition' command for Haskell code.

It relies on `ghc-lib-parser` to parse Haskell code. Supports only `*.cabal`-based projects.

The extension server stores its info at `~/.local/share/haskell-gtd-nl/` directory. Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.
`repos` directory is added to a working directory root as a `.repos` symlink (to prevent https://github.com/haskell/vscode-haskell/issues/480).

[!(example)](https://github.com/kr3v/gtd-nl-hs/assets/14293293/a5dc1f20-d343-4761-ad65-5af7d6cefe91)

Video notes:
1. When `go to definition` is supported, the definition is underlined. Otherwise, the command is not supported for given identifier.
2. 

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

### "Activation"
The extension actually starts working whenever a definition is requested from the VS Code. 
The extension uses Cabal (`cabal.project` files and etc) to figure out what `*.cabal` files are present in a directory.
Once all the `*.cabal` files are parsed (and cached), the extension figures out what Cabal 'entity' (library, executable, etc) that owns the file where the definition was initially requested.
For the matching 'entity' and all local libraries used by it, the extension clones the corresponding Cabal packages into the `repos` directory at the extension root via `cabal get`.

