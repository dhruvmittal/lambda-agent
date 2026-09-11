# lambdA (λ)

A fast, keyboard-driven terminal AI coding and diagnostics agent in Haskell ([Brick](https://github.com/jtdaugherty/brick) + STM).

---

## Design Principles

- **Clean Context**: Full interactive TUI history (thinking accordions, diffs, tool logs) is kept locally; reasoning is stripped before remote API calls to conserve tokens.
- **Dual-Gate Mode Safety (`/plan` vs `/exec`)**: In `/plan` mode, destructive tools (`edit_file`, write, mutating bash) are statically excluded from model schemas and blocked by the dispatcher. Switch instantly with `Alt+M` or `F2`.
- **Concurrent Subagents (STM)**: Specialist subagents run concurrently on lightweight Haskell green threads, coordinated and safely cancelled via STM with isolated contexts and strict turn budgets.
- **Out-of-Band (OOB) Compaction**: Large compiler and diagnostic dumps are spooled to `.lambda/artifacts/` with lightweight XML pointers passed to context.
- **Interactive Security Ledger**: Safe commands match glob allowlists; unapproved or destructive actions prompt a JIT modal (`[1] Always`, `[2] Once`, `[3] No`, `[4] Never`).
- **Model Context Protocol (MCP)**: Native stdio JSON-RPC client support for external tool servers.

---

## Configuration

Loaded from `.lambda/config.json` (workspace) or `~/.config/lambdA/config.json` (global). Supports `{ENV:VAR_NAME}` expansion and `.env` files. No provider default is assumed.

### Local (Ollama, vLLM, LM Studio)
Local endpoints require no API key:
```json
{
  "api_base_url": "http://localhost:11434/v1",
  "model_name": "qwen2.5-coder:32b",
  "models": ["qwen2.5-coder:32b", "deepseek-r1:14b"],
  "model_aliases": { "coder": "qwen2.5-coder:32b" }
}
```

### Remote (OpenAI / Compatible Providers)
```json
{
  "api_base_url": "https://api.openai.com/v1",
  "api_key": "{ENV:OPENAI_API_KEY}",
  "model_name": "gpt-4o",
  "context_limit": 128000
}
```

**Environment variables**: `LAMBDA_BASE_URL` (or `OPENAI_BASE_URL`), `LAMBDA_MODEL`, `LAMBDA_API_KEY` (or `OPENAI_API_KEY`), `CONTEXT_LIMIT`.

---

## Usage

```bash
cabal run lambda                  # Start fresh session
cabal run lambda -- -c            # Resume latest session
cabal run lambda -- -s <id>       # Resume session by ID
cabal run lambda -- --list-sessions
```

### Key Commands & Shortcuts

- **Mode & Model**:
  - `Alt+M` / `F2`: Toggle `/plan` (read-only) and `/exec` modes.
  - `/model [alias]`: Switch active model on the fly (`claude`, `r1`, `4o`, `qwen`, `o3`, or custom ID).
- **Session & Lineage**:
  - `/rewind [N]` (or `/undo`): Drop last $N$ turn-pairs (saved to in-memory undo stack).
  - `/fork [title]`: Branch active conversation into a new child session.
  - `/new`: Save current session and start fresh.
- **Macros & Subagents**:
  - `/prompt <name> [args]` (or `/p`): Run template from `.lambda/prompts/<name>.md` with `$input` substitution.
  - `/sub <id|main>`: Inspect subagent thoughts or return to main chat (`Alt+←` / `Alt+→`).
- **Navigation & Editing**:
  - `Tab`: Pure contextual completion (commands, sessions, subagents, prompt macros, file paths).
  - `Ctrl+C`: Clear draft line / interrupt active generation.
  - `Ctrl+D`: Exit on empty line / delete character forward.
  - `Alt+H` / `F1`: Toggle Intelligence HUD.
  - `Ctrl+T`: Toggle thinking block expansion.

---

## Build & Install

### Prerequisites
- GHC 9.6+ and Cabal 3.8+ (or [Nix](https://nixos.org/) with flakes enabled)
- C library: `zlib`

### Via Nix
```bash
# Enter development shell
nix develop

# Build package (output in ./result/bin/lambda)
nix build

# Install binary to user profile
nix profile install .
```

### Via Cabal
```bash
# Build the executable
cabal build exe:lambda

# Install binary to ~/.cabal/bin (ensure it's in your $PATH)
cabal install exe:lambda --overwrite-policy=always

# Run 42-stage invariant test suite
cabal test
```

