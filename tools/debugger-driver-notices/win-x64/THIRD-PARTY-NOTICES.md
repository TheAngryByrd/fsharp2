# netcoredbg 3.2.0-1092 Windows x64 notices

This notice bundle accompanies the reviewed `netcoredbg-win64.zip` asset with
SHA-256 `3c410a45fa502415203a94fcb88654af65bf8e3dac158a5527a722e7a6b9274a`.

| Shipped file | Component and pinned source identity | Verbatim license |
| --- | --- | --- |
| `netcoredbg.exe` | Samsung netcoredbg `9744e1f051866215611b8440c638042aa2aa2f72` | `LICENSE.netcoredbg.txt` |
| `netcoredbg.exe` | Bundled nlohmann/json at the same netcoredbg commit | `LICENSE.nlohmann-json.txt` |
| `netcoredbg.exe` | Bundled linenoise-ng at the same netcoredbg commit | `LICENSE.linenoise-ng.txt` |
| `ManagedPart.dll` | Samsung netcoredbg `9744e1f051866215611b8440c638042aa2aa2f72` | `LICENSE.netcoredbg.txt` |
| `Microsoft.CodeAnalysis.dll` | Roslyn `281ac90b8b5dd9fd923a353afd4af74f3246ca5c` | `LICENSE.roslyn.txt` |
| `Microsoft.CodeAnalysis.CSharp.dll` | Roslyn `281ac90b8b5dd9fd923a353afd4af74f3246ca5c` | `LICENSE.roslyn.txt` |
| `Microsoft.CodeAnalysis.Scripting.dll` | Roslyn `281ac90b8b5dd9fd923a353afd4af74f3246ca5c` | `LICENSE.roslyn.txt` |
| `Microsoft.CodeAnalysis.CSharp.Scripting.dll` | Roslyn `281ac90b8b5dd9fd923a353afd4af74f3246ca5c` | `LICENSE.roslyn.txt` |
| `dbgshim.dll` | dotnet/diagnostics `d7b455b46332b31fd9ba3a3f3e020387984c511a` | `LICENSE.dbgshim.txt` |

The component commits come from embedded product-version metadata in the
reviewed binaries, except for the native executable identity reported by
`netcoredbg --buildinfo`. Build-only and test-only source dependencies absent
from the reviewed seven-file Windows archive are not redistributed here.

Pinned sources:

- <https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/LICENSE>
- <https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/third_party/json/LICENSE.MIT>
- <https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/third_party/linenoise-ng/LICENSE>
- <https://github.com/dotnet/roslyn/blob/281ac90b8b5dd9fd923a353afd4af74f3246ca5c/License.txt>
- <https://github.com/dotnet/diagnostics/blob/d7b455b46332b31fd9ba3a3f3e020387984c511a/LICENSE.TXT>
