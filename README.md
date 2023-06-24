# Haskell 'go to (non-local) definitions' extension
Both 'server' and 'front-end' (VS Code extension for now).

Relies on `cpphs` and `haskell-src-exts` to parse Haskell code.
Supports only `*.cabal`-based projects.

Extension server stores its info at `~/.local/share/haskell-gtd-extension-server-root` directory. Logs are not rotated (yet). Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.

[!(example)](https://github.com/kr3v/gtd-nl-hs/assets/14293293/a5dc1f20-d343-4761-ad65-5af7d6cefe91)

Video notes:
1. `data`, `class`, `instance` are not supported (to be implemented, back-end).
2. Qualified imports are not supported (to be implemented, back-end).
3. Operators are not supported (to be implemented, extension).
4. The extension only works in the workspace directory (bug, back-end).

General notes:
1. Certain files are not yet supported (for example, in certain cases of using non-default infix operators).
2. TBD

## How to run
1. `cabal run haskell-gtd-server`
2. `cd extension; code .`
3. Start debugging the extension via F5.

## Help
Code review and suggestions are requested.
