# XParsec as a parser design reference

Status: decision note, 2026-09-29

Scope: whether FSharp2 adopts XParsec or XParsec.FSharp for the Issue #29 parser

## Decision

FSharp2 uses the XParsec blog series as a design reference for layout and operator parsing. FSharp2 does not take a dependency on XParsec or XParsec.FSharp in the production compiler.

## Sources

- The series "Why build an F# parser with XParsec?" by Rob Lenders, posts 1 to 8, starts at <https://robertlenders.com/blog/xparsec-01-prologue>. Markdown copies of the posts are at `https://robertlenders.com/blog/<slug>.md`.
- XParsec: <https://github.com/roboz0r/XParsec>. The license is MIT. XParsec 1.0.0 is on NuGet.
- XParsec.FSharp: `src/XParsec.FSharp` on the `fsharp` branch. It is not published as a package.

## Facts that decide this

- **Diagnostic target.** XParsec.FSharp targets zero diagnostics on valid input, and an error node that lets parsing continue on invalid input. FSharp2 must match the Compatibility Oracle diagnostics exactly for code, text, range, order, and cascade behavior (FS Diagnostic Compatibility). Most of the Issue #29 recovery work proves that behavior. An adopted parser needs the same work again on a different syntax tree.
- **Availability.** XParsec.FSharp is unpublished source on a branch, with one author. FSharp2 would have to copy it into the repository. That conflicts with ADR 0001, which requires newly written production code.
- **NativeAOT.** No evidence shows that XParsec.FSharp passes the ADR 0002 NativeAOT gates.
- **Performance.** The prologue benchmarks show XParsec.FSharp lexing plus parsing at about 2 times the Oracle parser time and about 12 times its allocation on a large file. ADR 0017 selects memory techniques by benchmark, so a dependency with this profile needs a benchmark that justifies it.

## What FSharp2 takes from the series

These posts match open Issue #29 layout and operator work:

- Post 4, the lexical filter. It inserts the layout tokens that the parser consumes.
- Post 6, the offside context stacks. Each context kind has its own offside line and its own closing rules.
- Post 7, the permitted undentations. These are the exceptions to the offside rule.
- Post 5, Pratt parsing for F# operators. It covers precedence, associativity, and the split of fused operators.

Each rule that FSharp2 adopts from these posts must still pass an Oracle probe and a conformance row, as the Issue #29 brief requires.

## Open option

XParsec core 1.0.0 is a general combinator library, separate from XParsec.FSharp. A future ticket can evaluate it for token-level parsing. That ticket must include a NativeAOT publish check and a benchmark under ADRs 0007 and 0017.
