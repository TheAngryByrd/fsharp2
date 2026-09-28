# Issue #29 parser probe

This tool compares the Issue #29 syntax parser with the Compatibility Oracle for a set of source files.

## Use

1. Put the probe sources in one directory. Add a `Last.fs` file with a named module when a case needs a later file.
2. Optional: add `cases.json` to give each case its files, language version, and target.
3. Run the script from PowerShell 7:

   ```powershell
   pwsh tools/Issue29ParserProbe/Invoke-ParserProbe.ps1 -ProbeDirectory <directory>
   ```

Without `cases.json`, each `.fs` and `.fsi` file except `Last.fs` is one case with the files `<file>` and `Last.fs`, language version `10.0`, and target `exe`.

A `cases.json` entry has this form:

```json
[
  { "name": "u_dot", "files": ["u_dot.fs", "Last.fs"], "languageVersion": "7.0", "target": "library" }
]
```

## Output

The script writes these files in the probe directory:

| File | Content |
| --- | --- |
| `cases.resolved.json` | The cases with default values filled in. |
| `oracle.json` | The Oracle diagnostics of each case from `fsc --parseonly --vserrors`. |
| `parser.json` | The parser diagnostics of each case from `Parser.parseCompilation`. |
| `compare.txt` | One status line per case, then the Oracle (`O`) and parser (`P`) lines of each case that is not exact. |

Each diagnostic line has the form `<file>(<start line>,<start column>,<end line>,<end column>): <severity> <code>: <message>`. The severity is `error` or `warning`, exactly as the Oracle prints `parse error` or `parse warning`. A message on more than one line keeps its line breaks.

The status of a case is one of these:

| Status | Meaning |
| --- | --- |
| `EXACT` | The parser lines equal the Oracle lines, in order. |
| `EXPLICIT` | The parser reports `FSC2P1001`, and each other parser line is an Oracle line. |
| `INVENTED` | The parser reports an FS line that the Oracle does not report. |
| `MISSING` | The parser misses an Oracle line and reports no `FSC2P1001`. |

The last output line gives the count of each status.

## Parser side

`tests/fsharp2.Tests/ParserProbe.fs` holds the parser side, because only `fsharp2.Tests` can see the internal parser. The script sets `FSHARP2_PARSER_PROBE_DIRECTORY` and runs the `Issue29.ParserProbe` test. The test list is empty when the variable is not set, so the normal test run does not include it.

## Limits

- Oracle diagnostics without a source range, such as FS0226, appear as `unparsed:` lines.
- Type-check evidence (`--typecheckonly`) is not part of this tool.
