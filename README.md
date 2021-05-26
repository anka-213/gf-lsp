# Language Server for Grammatical Framework

This language server provides some very basic IDE support for [Grammatical Framework](https://www.grammaticalframework.org/)

Currently, it only provides error messages for the currently opened file and it only checks when you save, so I recommend enabling auto-save in your editor ([VS Code](https://code.visualstudio.com/docs/editor/codebasics#_save-auto-save)).



## Installation

This currently requires a patched version of Grammatical Framework, so the easiest way to build it is using [nix](https://nixos.org/). I'll add support for other build workflows later.

There are a couple of ways to build it:
```
nix-shell
cabal install
```
or with
```
nix-env -if .
```
or
```
nix-env -if https://github.com/anka-213/gf-lsp/archive/main.tar.gz
```

## How to use

You need to enable LSP client in your editor to use this. For example, you can download the VS Code plugin at https://github.com/anka-213/gf-vscode.
For other editors, configure `.gf` files to be handled by `gf-lsp`.

After installing both the server and client, it should show error messages inline at the location of the error.