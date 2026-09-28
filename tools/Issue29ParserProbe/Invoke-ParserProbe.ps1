[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string] $ProbeDirectory,

    [string] $SdkRoot = (Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'fsharp2-sdk-10.0.110'),

    [switch] $SkipBuild
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$repositoryRoot = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$directory = (Resolve-Path $ProbeDirectory).Path
$dotnet = Join-Path $SdkRoot ($IsWindows ? 'dotnet.exe' : 'dotnet')
$fsc = Join-Path $SdkRoot 'sdk/10.0.110/FSharp/fsc.dll'

function Read-Cases {
    $casesPath = Join-Path $directory 'cases.json'

    $cases =
        if (Test-Path $casesPath) {
            Get-Content -Raw $casesPath | ConvertFrom-Json
        }
        else {
            Get-ChildItem -Path $directory -File |
                Where-Object { $_.Extension -in '.fs', '.fsi' -and $_.Name -ne 'Last.fs' } |
                Sort-Object Name |
                ForEach-Object { [pscustomobject] @{ name = $_.Name; files = @($_.Name, 'Last.fs') } }
        }

    foreach ($case in $cases) {
        [pscustomobject] [ordered] @{
            name = $case.name
            files = @($case.files)
            languageVersion = if ($case.PSObject.Properties['languageVersion']) { $case.languageVersion } else { '10.0' }
            target = if ($case.PSObject.Properties['target']) { $case.target } else { 'exe' }
        }
    }
}

function ConvertFrom-OracleOutput([string[]] $lines) {
    $pattern = '^(?<path>.+?)\((?<range>\d+,\d+,\d+,\d+)\): parse (?<severity>error|warning) (?<code>FS\d+): (?<message>.*)$'
    $diagnostics = [System.Collections.Generic.List[string]]::new()

    foreach ($line in $lines) {
        if ([string]::IsNullOrEmpty($line) -or $line -match 'FS0075') {
            continue
        }

        $match = [regex]::Match($line, $pattern)

        if ($match.Success) {
            $file = Split-Path -Leaf $match.Groups['path'].Value
            $diagnostics.Add("$file($($match.Groups['range'].Value)): $($match.Groups['severity'].Value) $($match.Groups['code'].Value): $($match.Groups['message'].Value)")
        }
        elseif ($diagnostics.Count -gt 0) {
            $diagnostics[$diagnostics.Count - 1] += "`n$line"
        }
        else {
            $diagnostics.Add("unparsed: $line")
        }
    }

    , $diagnostics.ToArray()
}

$cases = @(Read-Cases)
$cases | ConvertTo-Json -Depth 4 -AsArray | Set-Content -Encoding utf8NoBOM (Join-Path $directory 'cases.resolved.json')

$oracle = [ordered] @{}
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
Push-Location $directory

try {
    foreach ($case in $cases) {
        $arguments = @($fsc, '--parseonly', '--nologo', '--vserrors', '--utf8output', "--langversion:$($case.languageVersion)", "--target:$($case.target)") + $case.files
        $output = & $dotnet @arguments 2>&1 | ForEach-Object { "$_" }
        $oracle[$case.name] = ConvertFrom-OracleOutput $output
    }
}
finally {
    Pop-Location
}

$oracle | ConvertTo-Json -Depth 4 | Set-Content -Encoding utf8NoBOM (Join-Path $directory 'oracle.json')

$env:DOTNET_ROOT = $SdkRoot
$env:DOTNET_MULTILEVEL_LOOKUP = '0'
$env:PATH = "$SdkRoot$([IO.Path]::PathSeparator)$env:PATH"
$env:FSHARP2_PARSER_PROBE_DIRECTORY = $directory
$project = Join-Path $repositoryRoot 'tests/fsharp2.Tests/fsharp2.Tests.fsproj'

try {
    if (-not $SkipBuild) {
        dotnet build $project -c Release --no-restore -v quiet -nologo -clp:ErrorsOnly
        if ($LASTEXITCODE -ne 0) { throw 'The test project did not build.' }
    }

    $testLog = Join-Path $directory 'parser-test.log'
    dotnet test $project -c Release --no-restore --no-build --filter 'FullyQualifiedName~Issue29.ParserProbe' *> $testLog
    if ($LASTEXITCODE -ne 0) { throw "The parser probe test failed. See $testLog." }
}
finally {
    Remove-Item Env:FSHARP2_PARSER_PROBE_DIRECTORY
}

$parser = Get-Content -Raw (Join-Path $directory 'parser.json') | ConvertFrom-Json -AsHashtable
$report = [System.Collections.Generic.List[string]]::new()
$counts = [ordered] @{ EXACT = 0; EXPLICIT = 0; MISSING = 0; INVENTED = 0 }

foreach ($case in $cases) {
    $expected = @($oracle[$case.name])
    $actual = @($parser[$case.name])
    $invented = @($actual | Where-Object { $_ -notmatch ': error FSC2P1001: ' -and $expected -cnotcontains $_ })
    $explicit = @($actual | Where-Object { $_ -match ': error FSC2P1001: ' }).Count -gt 0

    $status =
        if (($expected -join "`0") -ceq ($actual -join "`0")) { 'EXACT' }
        elseif ($invented.Count -gt 0) { 'INVENTED' }
        elseif ($explicit) { 'EXPLICIT' }
        else { 'MISSING' }

    $counts[$status]++
    $report.Add("$($case.name)`t$status")

    if ($status -ne 'EXACT') {
        foreach ($line in $expected) { $report.Add("   O $line") }
        foreach ($line in $actual) { $report.Add("   P $line") }
    }
}

$report | Set-Content -Encoding utf8NoBOM (Join-Path $directory 'compare.txt')
($counts.GetEnumerator() | ForEach-Object { "$($_.Key)=$($_.Value)" }) -join ' '
