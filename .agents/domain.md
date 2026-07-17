# Domain Docs

Before exploring, read `CONTEXT.md` at the repo root when it exists and read ADRs in `.agents/docs/adr/` that touch the area being explored.

If these files do not exist, proceed silently. The domain-modeling skill creates them lazily when needed.

## File structure

Single-context repo:

- `CONTEXT.md`
- `.agents/docs/adr/`
- `src/`

Use the glossary vocabulary from `CONTEXT.md` when naming domain concepts. If an output conflicts with an ADR, call out that conflict explicitly rather than silently overriding it.
