# Haskell 'go to (non-local) definitions' extension
Both 'server' and 'front-end' (VS Code extension for now).

It relies on `cpphs` and `ghc-lib-parser` to parse Haskell code.
Supports only `*.cabal`-based projects.

The extension server stores its info at `~/.local/share/haskell-gtd-extension-server-root` directory. Logs are not rotated (yet?). Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.
`repos` directory is added to a working directory root as a `.repos` symlink (to prevent https://github.com/haskell/vscode-haskell/issues/480).

[!(example)](https://github.com/kr3v/gtd-nl-hs/assets/14293293/a5dc1f20-d343-4761-ad65-5af7d6cefe91)

Video notes:
1. When `go to definition` is supported, the definition is underlined. Otherwise, the command is not supported.
2. `data`, `class`, `instance` are not supported (to be implemented, back-end).
3. Qualified imports are not supported (to be implemented, back-end).
4. Operators are not supported (to be implemented, extension).
5. The extension only works in the workspace directory (bug, back-end).

General notes:
1. The extension works only in a project with a `*.cabal` file at its root.
2. The first attempt to perform a `go to definition` action might take some time, yet consequent attempts in the same VS Code session should take much less time.

## How to run
### Debug
1. `cabal run haskell-gtd-server`
2. `cd extension; code .`
3. Start debugging the extension via F5.
4. The extension should activate when a Haskell file gets opened.

### "Release"
1. Install the extension as `.vsix`.
2. Create a `~/.local/share/haskell-gtd-extension-server-root/` directory and put `haskell-gtd-server` and `haskell-gtd-package` binaries in it.
3. The extension should activate when a Haskell file gets opened.
