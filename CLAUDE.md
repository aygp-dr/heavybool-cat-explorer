# Project Instructions for AI Agents

This file provides instructions and context for AI coding agents working on this project.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->


## Build & Test

```bash
make run-examples    # Run all examples (default)
make test            # Run all tests
make clean           # Clean temporary files
make setup           # Setup directories
guile examples/example-name.scm  # Run specific example
```

## Architecture Overview

HeavyBool extends boolean values with explanatory metadata (Writer monad pattern).

```
src/
  heavybool.scm           # Core record type, ensure-heavy-bool, logical ops
  heavybool-quantifiers.scm  # forall-m, exists-m, any-m, all-m
  heavybool-monad.scm     # return-bool, bind-bool, kleisli-compose, fmap-bool
tests/
  run-tests.scm           # Test framework and all tests
examples/
  associativity-test.scm  # Property testing with counterexamples
  strange-loops.scm       # Self-referential structures
```

## Conventions & Patterns

- **Naming**: Use kebab-case for functions/variables (e.g., `heavy-bool`)
- **Modules**: Use `-m` suffix for monadic versions (e.g., `and-m`, `not-m`)
- **File Structure**: Implementation in `src/`, tests in `tests/`, examples in `examples/`
- **Error Handling**: Use `ensure-heavy-bool` for type checking
- **Documentation**: Literate programming in Org mode format
- **Imports**: Use relative paths with `load` for importing modules

## Anti-Goals (MUST NOT)

These are explicit prohibitions — agents MUST NOT violate these:

1. **MUST NOT** change monad law behavior without updating tests
2. **MUST NOT** remove error checks from `ensure-heavy-bool`, `forall-m`, `any-m`, `all-m`
3. **MUST NOT** modify `heavy-bool-value` or `heavy-bool-because` accessor behavior
4. **MUST NOT** change quantifier short-circuit semantics (forall stops on first false)
5. **MUST NOT** remove witness/counterexample annotation from quantifiers
6. **MUST NOT** change the reason accumulation order in `bind-bool` (append semantics)
7. **MUST NOT** add features without corresponding tests for both positive and negative cases
