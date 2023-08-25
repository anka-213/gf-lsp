# Language Server for Grammatical Framework

This language server provides some very basic IDE support for [Grammatical Framework](https://www.grammaticalframework.org/)

Currently, it only provides error messages for the currently opened file and it only checks when you save, so I recommend enabling auto-save in your editor ([VS Code](https://code.visualstudio.com/docs/editor/codebasics#_save-auto-save)).



## Installation

Only Mac and Linux is currently supported. Windows support will come in the future.

### Visual Studio Code

For VS Code, you can install the [Grammatical Framework Language Server](https://marketplace.visualstudio.com/items?itemName=anka-213.gf-vscode) extension and it will automatically install the language server for you.

### Emacs

Install the version of gf-lsp for your system according to the sections below, then install eglot and put this config in your emacs config file

```
(use-package eglot
  :ensure t
  :config
  (add-hook 'gf-mode-hook 'eglot-ensure)
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(gf-mode . ("gf-lsp" "--lsp"))))
```

### Linux and Intel Macs

Prebuilt binaries are available with installation instructions in the [latest release](https://github.com/anka-213/gf-lsp/releases).

On M1 or M2 macs you will need to build from source. See the next section.

### Building from source (necessary on M1 and M2 Macs)

This currently requires a patched version of Grammatical Framework, so the easiest way to build it is using [nix](https://nixos.org/).

Run the following commands in a shell

```
# Install the nix build system
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
# Ensure that the newly installed commands are available
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
# Build and install the GF Language Server
nix-env -if https://github.com/anka-213/gf-lsp/archive/main.tar.gz
```

After this you need to configure your editor to use `~/.nix-profile/bin/gf-lsp` as the language server for `.gf` files.


## Developing

You need to install the dependencies using nix.

First install nix:
```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```
Optionally use my binary cache to speed up builds:
```
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use anka-213
```
Finally enable the nix-shell, to get all the necessary dependencies for building:
```
nix-shell
```

From the new shell you can install it using
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
