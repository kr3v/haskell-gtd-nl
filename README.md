# Haskell 'go to (non-local)* definitions' VS Code extension

This repository contains a VS Code extension and a Haskell server that implements a 'go to definition' command for Haskell code (for both local and non-local (remote libraries, local `cabal.project` dependencies)) definitions.

\* "non-local" - check this [issue](https://github.com/haskell/haskell-language-server/issues/708). HLS does not support 'go to definition' for non-local symbols (like `base`, `containers`, etc.). This extension attempts to fill this gap (hopefully, temporarily).

The extension:
- supports only `*.cabal`-based projects,
- supports multi-folder VS Code workspaces, as long as either `*.cabal` or `cabal.project` files are present in the folders' roots,
- is likely to work only on Linux (it was not tested on other platforms).

## How to install
1. Install the VS Code extension (`through the `*.vsix` or directly [dbaynak.haskell-gtd-nl](https://marketplace.visualstudio.com/items?itemName=dbaynak.haskell-gtd-nl) from either VS Code "extensions" or the marketplace).
2. Clone this repository. Execute `cabal install` in its root directory.
```shell
git clone https://github.com/kr3v/haskell-gtd-nl
cd haskell-gtd-nl
cabal update
cabal install
```
3. The extension should activate when a Haskell file gets opened in VS Code.

It would take some time for the extension to fetch and parse all the dependencies on the first start (it takes minutes). There would be no long pauses like the first initialization after it.

Notes:
- on Ubuntu22, I had to install `zlib1g-dev` package for `cabal install` to succeed;
- on Fedora 34, `zlib-devel` should be installed.

If HLS does not work in your project, enable `haskell-gtd-nl.extension.disable-local-definitions-when-hls-is-active` setting in VS Code extension for 'local' definitions.

## Performance
The first attempt to perform a `go to definition` action will take time (to fetch all the dependencies, to parse & cache them), yet consequent attempts should take much less time.

When you save a Haskell file, the extension will re-parse it, its module and all the modules that depend on it. In case you save a Cabal file, then all the directory cache is dropped.
The re-parsing process is quite fast, in my experience it takes up to a few seconds.

Overall, the memory usage should be low:
- ~100-150 MiB when serving after initialization,
- ~4 GiB during initialization / after a cache drop, but tunable.

`max_live_bytes` is ~470 MiB for this repo (including parsing _all_ the dependencies).

By default, the `--dynamic-memory-usage` parameter is enabled for the server.
If there are less than 8/4/2 GiB of RAM available, the parsing process `-A` parameters takes values of 64/16/4 `M` instead of the default `-A128M`. The intention here is to reduce memory usage when there is not much memory available by sacrificing performance (which is heavily dependent on the GC and RAM performance).

My advice would be to let the parser do its work during the first initialization and only then tighten, if required, its memory usage by having a custom `-A` setting with `--dynamic-memory-usage` disabled.

## HLS
### VS Code extension `haskell-gtd-nl.extension.disable-local-definitions-when-hls-is-active` setting
In case HLS is enabled and is actually working (i.e. it provides 'go-to definitions' for local symbols), the default extension behavior is to not provide such definitions. This extension can produce local definitions, but they are different from the HLS' ones, i.e. you might see two different definitions for the same symbol. This is a UX-only issue, both definitions are correct, but it might be confusing.

In case you want to see local definitions even when HLS is active, you can set `haskell-gtd-nl.extension.disable-local-definitions-when-hls-is-active` to `false` in VS Code extension settings.

This is an extension-only setting, the server does not know about it.

### Multiple HLS instances when multiple directories are opened with VS Code (aka [vscode-haskell #480](https://github.com/haskell/vscode-haskell/issues/480))
The extension server stores its info at `~/.local/share/haskell-gtd-nl` directory. Cabal packages are cloned into `repos` directory at the extension root via `cabal get`.
`repos` directory is added to a working directory root as a `.repos` symlink and all the extension accesses to `repos` directory happen through this symlink. This allows to avoid having multiple `hls` instances when multiple directories are opened with VS Code.

In other words, when you `go to definition` on a `return` symbol (from `Prelude.Monad`), the server would clone a proper `base` package into `~/.local/share/haskell-gtd-nl/repos` directory, yet the extension would access it through `<project root>/.repos` symlink (opening something like `<project root>/.repos/base-<version>/src/GHC/Base.hs` file).

If the extension was to open the `repos` directory directly, then `hls` would create a separate instance for each opened directory (which is not what we want).

### Why not implement this feature in HLS?
The main concern was that I was not sure if an in-HLS implementation I could come up with would be accepted by the HLS team. As far as I know, there are 2 PRs that implement similar functionality, but one was not accepted and the other one is in review (and it was created after I started working on this extension).
I do not want my extension to replace HLS, I want it to be a solution until HLS gets the same functionality.

The other concern is that HLS does not support _certain_ projects (HLS does not work in `base`; HLS does not 'see' `cabal.project` configuration). My initial desire was the ability to browse the `base` package.
