# lambdA (lambda-agent)

A terminal-based AI agent interface written in Haskell.

## Overview

`lambdA` provides a rich text user interface (TUI) for interacting with AI agents, featuring:
- Tool execution and thinking/chain-of-thought rendering
- Subprocess invocation with permission bubbling and approval modals
- Terminal UI built with the Brick library

## Development

This project is built with Haskell and managed via Nix and Cabal.

To enter the development shell (if you use Nix):
```bash
nix develop
```

To build and run:
```bash
cabal run lambda
```
