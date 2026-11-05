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

Loaded from `.lambda/config.json` (workspace) or `~/.config/lambdA/config.json` (global). Supports `{ENV:VAR_NAME}` template expansion and `.env` / `.env.local` files. No provider default is assumed.

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

### On-Prem & Portkey AI Gateway
Supports custom HTTP headers (such as Portkey virtual keys, gateway configs, or corporate bearer tokens), context window overrides, and strict on-prem model isolation:
```json
{
  "api_base_url": "https://portkey.internal.work.com/v1",
  "api_key": "{ENV:PORTKEY_API_KEY}",
  "custom_headers": {
    "x-portkey-virtual-key": "{ENV:PORTKEY_VIRTUAL_KEY}",
    "x-portkey-config": "enterprise-coding"
  },
  "model_name": "company-qwen-coder-32b",
  "models": [
    "company-qwen-coder-32b",
    "company-llama-70b"
  ],
  "model_limits": {
    "company-qwen-coder-32b": 32768,
    "company-llama-70b": 65536
  },
  "context_limit": 32768
}
```
> [!NOTE]
> When `models` is specified or when using a non-OpenRouter endpoint, unconfigured cloud models (Claude, DeepSeek, GPT-4o) are suppressed from both TUI auto-completion and Zed's ACP model picker, complying with strict on-prem corporate privacy policies.

### Remote (OpenAI / Compatible Providers)
```json
{
  "api_base_url": "https://api.openai.com/v1",
  "api_key": "{ENV:OPENAI_API_KEY}",
  "model_name": "gpt-4o",
  "context_limit": 128000
}
```

### Security & Automated Approvals
lambdA employs an interactive JIT security ledger. Destructive operations (mutating bash commands, edits, writes) or actions touching paths outside the workspace boundary prompt for authorization. To pre-authorize routine operations without manual approval prompts (especially useful in headless ACP or remote SSH sessions):
```json
{
  "always_allow_globs": [
    "read_file*",
    "list_directory*",
    "grep_search*",
    "edit_file*",
    "write_file*",
    "git *",
    "cabal test*",
    "cargo test*"
  ],
  "always_deny_globs": [
    "rm -rf /*",
    "mkfs*",
    ":(){ :|:& };:"
  ]
}
```

### Full Configuration Reference

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `api_base_url` | `string` | `""` | Base URL for OpenAI-compatible endpoint (e.g. `http://localhost:11434/v1`). |
| `api_key` | `string` | `""` | API key. Supports `{ENV:KEY_NAME}` placeholder syntax. |
| `model_name` | `string` | `""` | Default active model ID. |
| `models` | `string[]` | `[]` | Explicit model whitelist. When set, hides unconfigured cloud models. |
| `model_aliases` | `object` | `{}` | Friendly shorthands mapping to full model names (e.g. `{"coder": "qwen2.5-coder:32b"}`). |
| `model_limits` | `object` | `{}` | Per-model token context window limits (e.g. `{"my-model": 32768}`). |
| `context_limit` | `integer` | `128000` | Fallback context window size in tokens for compaction triggers. |
| `custom_headers` | `object` | `{}` | Key-value HTTP headers sent with every completion and `/models` request. |
| `always_allow_globs` | `string[]` | *safe reads* | Glob patterns pre-approved to execute without prompting the security ledger. |
| `always_deny_globs` | `string[]` | *destructive* | Glob patterns strictly blocked from execution across all modes. |
| `max_turn_budget` | `integer` | `30` | Maximum autonomous turns before pausing for user input. |
| `max_saved_sessions`| `integer` | `50` | Maximum session history records retained on disk. |
| `mcp_servers` | `object` | `{}` | External Model Context Protocol servers configured via stdio JSON-RPC. |
| `specialists` | `object` | *builtins* | Custom specialist subagent configurations with role, budget, and capabilities. |
| `theme` | `string` | `"default"` | Active TUI theme (`default`, `gruvbox`, `dracula`, `nord`, `solarized-dark`, etc.). |
| `mouse_support` | `boolean` | `true` | Enable/disable mouse clicking, scrolling, and accordion toggle in TUI. |

**Environment variables**: `LAMBDA_BASE_URL` (or `OPENAI_BASE_URL`), `LAMBDA_MODEL`, `LAMBDA_API_KEY` (or `OPENAI_API_KEY`), `CONTEXT_LIMIT`, `LAMBDA_THEME`.

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

## Zed Editor Integration (Agent Client Protocol)

lambdA provides native headless **ACP (Agent Client Protocol)** support via `lambda --acp` (or `lambda -a`). When run in ACP mode, lambdA bypasses the terminal UI completely, reserving `stdout` strictly for JSON-RPC 2.0 streaming messages while routing internal engine diagnostics to `stderr`.

### Features Supported
- **Extended Session Lifecycle**: Full support for `session/new`, `session/load`, `session/list` (browser / session picker), `session/delete`, `session/close`, and `session/set_config_option` (dynamic model selection).
- **Rich Notifications**:
  - `plan`: Structured checklist updates (`pending`, `in_progress`, `completed`) reflecting internal goals and planning loops.
  - `session_usage_update`: Live prompt/completion token counts and estimated USD cost per turn.
  - `session_info_update`: Dynamic session title updates.
- **Client-Side Delegation & Full-Duplex RPC**: `session/request_permission` delegates tool authorization prompts to native editor modals with fallback to local security policies.
- **Interactive Mode Switching**: Seamless switching between `/plan` (safe read-only inspection) and `/exec` (full modification tools).

### Zed Configuration
Install the `lambda` binary to your `$PATH` (e.g. via `nix profile install .` or `cabal install`), then add lambdA to your Zed configuration (`~/.config/zed/settings.json` or `Preferences: Open Settings`):

```json
{
  "agent_servers": {
    "lambdA": {
      "type": "custom",
      "command": "lambda",
      "args": ["--acp"]
    }
  }
}
```

Once configured:
- Open the Zed Assistant panel (`Ctrl+?` or `Cmd+?`).
- Select **lambdA** as your active agent server.
- The assistant streams thoughts, tool executions, and turn completions natively, displaying live plans, token usage telemetry, and session management.

### Remote Development & SSH Permissions

When connecting to a remote machine via Zed's Remote SSH, external ACP agents can encounter permission or path confinement errors if the remote working directory or permissions are not aligned:

1. **Working Directory Alignment**:
   Ensure Zed launches lambdA inside your target project directory rather than `$HOME`, preventing in-project files from being flagged as `outside_workspace`:
   ```json
   {
     "agent_servers": {
       "lambdA": {
         "type": "custom",
         "command": "bash",
         "args": ["-c", "cd /path/to/project && exec lambda --acp"]
       }
     }
   }
   ```

2. **Pre-Authorizing Permissions**:
   If remote SSH client popups do not render or prompt modals are blocked by editor policy, pre-approve tool operations in your remote repository's `.lambda/config.json`:
   ```json
   {
     "always_allow_globs": [
       "read_file*",
       "edit_file*",
       "write_file*",
       "list_directory*",
       "grep_search*",
       "git *"
     ]
   }
   ```
   This allows lambdA to execute file edits and safe tool inspections without waiting for round-trip UI modal confirmations over SSH.

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

# Run 56-stage invariant test suite
cabal test
```

