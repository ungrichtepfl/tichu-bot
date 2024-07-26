# Haskell Tichu Bot

This is a Tichu bot written in Haskell.

## Installation

You need Stack, Cabal and GHC to compile and run it. On **Linux, macOS, FreeBSD
or WSL2** you can use [GHCup](https://www.haskell.org/ghcup/) to install all tools
needed (you need to have `curl` installed, on Ubuntu use `sudo apt install curl`):

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

To install Stack, Cabal and GHC then use:

```shell
ghcup tui
```

On **Windows** check out [this page](https://www.haskell.org/downloads/) for
installation instruction.

## Usage

To play Tichu run:

```shell
stack run
```

To run all the unit tests use:

```shell
stack test
```

## Development

Please use `fourmolu>=0.16.2.0` as a formatter (see their [GitHub page](https://github.com/fourmolu/fourmolu?tab=readme-ov-file#usage).

You can then run it using:

```shell
fourmolu -i -r src test
```
