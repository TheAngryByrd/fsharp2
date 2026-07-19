[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string] $ArchivePath,
    [string] $ArtifactsPath = (Join-Path $PSScriptRoot '..\artifacts\debugger-driver-probe'),
    [ValidateRange(2, 10)]
    [int] $AttemptCount = 2
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Get-Sha256Text {
    param([Parameter(Mandatory = $true)][string] $Text)

    $bytes = [System.Text.Encoding]::UTF8.GetBytes($Text)
    $hash = [System.Security.Cryptography.SHA256]::HashData($bytes)
    return [System.Convert]::ToHexString($hash).ToLowerInvariant()
}

function Get-Identity {
    param(
        [Parameter(Mandatory = $true)][string] $Path,
        [string] $RelativeTo
    )

    $item = Get-Item -LiteralPath ([System.IO.Path]::GetFullPath($Path))
    $displayPath = $item.FullName

    if (-not [string]::IsNullOrWhiteSpace($RelativeTo)) {
        $relative = [System.IO.Path]::GetRelativePath(
            [System.IO.Path]::GetFullPath($RelativeTo),
            $item.FullName)
        $parentPrefix = '..' + [System.IO.Path]::DirectorySeparatorChar

        if (-not [System.IO.Path]::IsPathRooted($relative) -and
            $relative -ne '..' -and
            -not $relative.StartsWith($parentPrefix, [System.StringComparison]::Ordinal)) {
            $displayPath = $relative.Replace('\', '/')
        }
    }

    return [pscustomobject]@{
        path = $displayPath
        length = $item.Length
        sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $item.FullName).Hash.ToLowerInvariant()
    }
}

function Assert-LockedFile {
    param(
        [Parameter(Mandatory = $true)][string] $Path,
        [Parameter(Mandatory = $true)] $Expected,
        [Parameter(Mandatory = $true)][string] $Description,
        [string] $RelativeTo
    )

    $identity = Get-Identity -Path $Path -RelativeTo $RelativeTo

    if ($identity.length -ne [long] $Expected.length -or
        $identity.sha256 -ne [string] $Expected.sha256) {
        throw "$Description identity mismatch. Expected $($Expected.length)/$($Expected.sha256); got $($identity.length)/$($identity.sha256)."
    }

    return $identity
}

function Assert-ChildPath {
    param(
        [Parameter(Mandatory = $true)][string] $Parent,
        [Parameter(Mandatory = $true)][string] $Child
    )

    $parentPath = [System.IO.Path]::GetFullPath($Parent)
    $childPath = [System.IO.Path]::GetFullPath($Child)
    $relative = [System.IO.Path]::GetRelativePath($parentPath, $childPath)
    $parentPrefix = '..' + [System.IO.Path]::DirectorySeparatorChar

    if ($relative -eq '.' -or
        [System.IO.Path]::IsPathRooted($relative) -or
        $relative -eq '..' -or
        $relative.StartsWith($parentPrefix, [System.StringComparison]::Ordinal)) {
        throw "Refusing to mutate '$childPath' because it is not a child of '$parentPath'."
    }
}

function Get-IsolatedBuildArguments {
    param(
        [Parameter(Mandatory = $true)][string] $Project,
        [Parameter(Mandatory = $true)][string] $Configuration,
        [Parameter(Mandatory = $true)][string] $OutputBase,
        [Parameter(Mandatory = $true)][string] $IntermediateBase,
        [string[]] $AdditionalArguments = @()
    )

    return @(
        'build'
        $Project
        '-t:Rebuild'
        '-c'
        $Configuration
        '--no-incremental'
        $AdditionalArguments
        "-p:BaseOutputPath=$OutputBase"
        "-p:IntermediateOutputPath=$IntermediateBase"
        "-p:MSBuildProjectExtensionsPath=$IntermediateBase"
        '-p:RestoreLockedMode=true'
        '-p:NuGetAudit=false'
        '-p:Deterministic=true'
        '-v:minimal'
    )
}

$repositoryRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$lockPath = Join-Path $PSScriptRoot 'debugger-driver.lock.json'
$driverLock = Get-Content -Raw -LiteralPath $lockPath | ConvertFrom-Json

if ($driverLock.schemaVersion -ne 2) {
    throw "Unsupported debugger-driver lock schema '$($driverLock.schemaVersion)'."
}

$rid = if ($IsWindows -and [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'X64') {
    'win-x64'
}
elseif ($IsLinux -and [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'X64') {
    'linux-x64'
}
elseif ($IsLinux -and [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'Arm64') {
    'linux-arm64'
}
elseif ($IsMacOS -and [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'Arm64') {
    'osx-arm64'
}
else {
    throw 'The pinned driver has no reviewed asset for this OS/architecture.'
}

$assets = @($driverLock.driver.assets | Where-Object { $_.rid -eq $rid })

if ($assets.Count -ne 1) {
    throw "The driver lock must contain exactly one asset for $rid."
}

$asset = $assets[0]

if ($asset.sha256 -notmatch '^[a-f0-9]{64}$') {
    throw "The driver lock contains an invalid SHA-256 for $rid."
}

$artifactsRoot = [System.IO.Path]::GetFullPath($ArtifactsPath)
$driverParent = [System.IO.Path]::GetFullPath((Join-Path $artifactsRoot 'driver'))
$driverRoot = [System.IO.Path]::GetFullPath((Join-Path $driverParent $asset.sha256))
$mappedSourceRoot = [System.IO.Path]::GetFullPath((Join-Path $artifactsRoot (Join-Path 'source' $rid)))
$evidenceRoot = [System.IO.Path]::GetFullPath((Join-Path $artifactsRoot (Join-Path 'evidence' $rid)))
$buildRoot = [System.IO.Path]::GetFullPath((Join-Path $artifactsRoot (Join-Path 'build' $rid)))
$mirrorRoot = [System.IO.Path]::GetFullPath((Join-Path $artifactsRoot (Join-Path 'mirror' $asset.sha256)))

foreach ($mutablePath in @($driverRoot, $mappedSourceRoot, $evidenceRoot, $buildRoot, $mirrorRoot)) {
    Assert-ChildPath -Parent $artifactsRoot -Child $mutablePath
}

if (-not ($asset.PSObject.Properties.Name -contains 'noticeBundle') -or
    -not ($asset.PSObject.Properties.Name -contains 'oracleToolchain') -or
    -not ($asset.PSObject.Properties.Name -contains 'focusedReplay')) {
    throw "The $rid cell has no reviewed notice, Oracle toolchain, and focused-replay lock."
}

$providedArchivePath = [System.IO.Path]::GetFullPath($ArchivePath)
$providedArchiveIdentity = Assert-LockedFile `
    -Path $providedArchivePath `
    -Expected $asset `
    -Description 'Driver archive'
$noticeSourceRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot $asset.noticeBundle.root))
$noticeSourcePaths = @(
    $asset.noticeBundle.files | ForEach-Object {
        [System.IO.Path]::GetFullPath((Join-Path $noticeSourceRoot $_.path))
    }
)
$noticeSourceIdentities = @(
    for ($index = 0; $index -lt $asset.noticeBundle.files.Count; $index++) {
        Assert-LockedFile `
            -Path $noticeSourcePaths[$index] `
            -Expected $asset.noticeBundle.files[$index] `
            -Description "Driver notice '$($asset.noticeBundle.files[$index].path)'" `
            -RelativeTo $repositoryRoot
    }
)

foreach ($replacePath in @($mappedSourceRoot, $evidenceRoot, $buildRoot, $mirrorRoot)) {
    if (Test-Path -LiteralPath $replacePath) {
        Remove-Item -Force -Recurse -LiteralPath $replacePath
    }
}

New-Item -ItemType Directory -Force -Path `
    $driverParent, $mappedSourceRoot, $evidenceRoot, $buildRoot, $mirrorRoot | Out-Null

$mirrorArchivePath = Join-Path $mirrorRoot $asset.fileName
$mirrorNoticeRoot = Join-Path $mirrorRoot 'notices'
New-Item -ItemType Directory -Force -Path $mirrorNoticeRoot | Out-Null
Copy-Item -Force -LiteralPath $providedArchivePath -Destination $mirrorArchivePath
$noticeMirrorIdentities = @(
    for ($index = 0; $index -lt $asset.noticeBundle.files.Count; $index++) {
        $mirrorNoticePath = Join-Path $mirrorNoticeRoot $asset.noticeBundle.files[$index].path
        Copy-Item -Force -LiteralPath $noticeSourcePaths[$index] -Destination $mirrorNoticePath
        Assert-LockedFile `
            -Path $mirrorNoticePath `
            -Expected $asset.noticeBundle.files[$index] `
            -Description "Materialized driver notice '$($asset.noticeBundle.files[$index].path)'" `
            -RelativeTo $artifactsRoot
    }
)

$ArchivePath = [System.IO.Path]::GetFullPath($mirrorArchivePath)
$archiveIdentity = Assert-LockedFile `
    -Path $ArchivePath `
    -Expected $asset `
    -Description 'Materialized driver archive' `
    -RelativeTo $artifactsRoot

$relativeDriverRoot = [System.IO.Path]::GetRelativePath($driverParent, $driverRoot)

if ([System.IO.Path]::IsPathRooted($relativeDriverRoot) -or $relativeDriverRoot.StartsWith('..')) {
    throw "Refusing to replace driver extraction outside '$driverParent'."
}

# Never trust a prior extraction. The reviewed archive is re-expanded after its
# exact byte identity is checked, so stale or modified dependencies cannot hide
# behind an existing executable.
if (Test-Path -LiteralPath $driverRoot) {
    Remove-Item -Force -Recurse -LiteralPath $driverRoot
}

New-Item -ItemType Directory -Force -Path $driverRoot | Out-Null

if ($asset.fileName.EndsWith('.zip', [System.StringComparison]::OrdinalIgnoreCase)) {
    Expand-Archive -LiteralPath $ArchivePath -DestinationPath $driverRoot
}
else {
    & tar -xzf $ArchivePath -C $driverRoot

    if ($LASTEXITCODE -ne 0) {
        throw "tar failed with exit code $LASTEXITCODE."
    }
}

$driverExecutable = Join-Path $driverRoot $asset.executable

if (-not (Test-Path -LiteralPath $driverExecutable)) {
    throw "The reviewed archive did not contain '$($asset.executable)'."
}

if (-not $IsWindows) {
    & chmod +x $driverExecutable

    if ($LASTEXITCODE -ne 0) {
        throw "chmod failed with exit code $LASTEXITCODE."
    }
}

$extractedFiles = @(
    Get-ChildItem -LiteralPath $driverRoot -File -Recurse |
        Sort-Object FullName |
        ForEach-Object {
            [pscustomobject]@{
                path = [System.IO.Path]::GetRelativePath($driverRoot, $_.FullName).Replace('\', '/')
                length = $_.Length
                sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $_.FullName).Hash.ToLowerInvariant()
            }
        }
)

$buildInfoLines = @(& $driverExecutable --buildinfo 2>&1)

if ($LASTEXITCODE -ne 0) {
    throw "netcoredbg --buildinfo failed with exit code $LASTEXITCODE."
}

$dotnetCommand = Get-Command dotnet -CommandType Application
$dotnetExecutable = [System.IO.Path]::GetFullPath($dotnetCommand.Source)
$dotnetRoot = Split-Path -Parent $dotnetExecutable
$sdkVersionLines = @(& $dotnetExecutable --version 2>&1)

if ($LASTEXITCODE -ne 0 -or $sdkVersionLines.Count -ne 1) {
    throw 'dotnet --version did not produce one successful SDK identity.'
}

$sdkVersion = $sdkVersionLines[0].Trim()

if ($sdkVersion -ne [string] $asset.oracleToolchain.sdkVersion) {
    throw "Oracle SDK identity mismatch. Expected '$($asset.oracleToolchain.sdkVersion)'; resolved '$sdkVersion'."
}

$dotnetHostIdentity = Assert-LockedFile `
    -Path $dotnetExecutable `
    -Expected $asset.oracleToolchain.dotnetHost `
    -Description 'dotnet host'
$sdkRoot = [System.IO.Path]::GetFullPath((Join-Path $dotnetRoot (Join-Path 'sdk' $sdkVersion)))
$sdkFileIdentities = @(
    $asset.oracleToolchain.sdkFiles | ForEach-Object {
        $sdkFilePath = Join-Path $sdkRoot ($_.relativePath.Replace('/', [System.IO.Path]::DirectorySeparatorChar))
        Assert-LockedFile `
            -Path $sdkFilePath `
            -Expected $_ `
            -Description "Oracle SDK file '$($_.relativePath)'" `
            -RelativeTo $sdkRoot
    }
)
$dotnetInfoLines = @(& $dotnetExecutable --info 2>&1)

if ($LASTEXITCODE -ne 0) {
    throw "dotnet --info failed with exit code $LASTEXITCODE."
}

$fixtureRoot = Join-Path $repositoryRoot 'tests\FSharp2.DebuggerFixture'
$fixtureSource = Join-Path $fixtureRoot 'Program.fs'
$mappedSource = Join-Path $mappedSourceRoot 'Program.fs'
Copy-Item -Force -LiteralPath $fixtureSource -Destination $mappedSource
[System.IO.File]::SetAttributes($mappedSource, [System.IO.FileAttributes]::ReadOnly)

$fixtureProject = Join-Path $fixtureRoot 'FSharp2.DebuggerFixture.fsproj'
$separator = [System.IO.Path]::DirectorySeparatorChar
$fixtureOutputBase = [System.IO.Path]::GetFullPath((Join-Path $buildRoot 'fixture\bin')) + $separator
$fixtureIntermediateBase = [System.IO.Path]::GetFullPath((Join-Path $buildRoot 'fixture\obj')) + $separator
$fixtureBuildArguments = @(
    Get-IsolatedBuildArguments `
        -Project $fixtureProject `
        -Configuration 'Debug' `
        -OutputBase $fixtureOutputBase `
        -IntermediateBase $fixtureIntermediateBase `
        -AdditionalArguments @("-p:PathMap=$fixtureRoot=$mappedSourceRoot")
)
& $dotnetExecutable @fixtureBuildArguments

if ($LASTEXITCODE -ne 0) {
    throw "Debugger fixture build failed with exit code $LASTEXITCODE."
}

$program = Join-Path $fixtureOutputBase 'Debug\net10.0\FSharp2.DebuggerFixture.dll'
$pdb = Join-Path $fixtureOutputBase 'Debug\net10.0\FSharp2.DebuggerFixture.pdb'
$probeProject = Join-Path $repositoryRoot 'tools\FSharp2.DebuggerProbe\FSharp2.DebuggerProbe.csproj'
$probeOutputBase = [System.IO.Path]::GetFullPath((Join-Path $buildRoot 'probe\bin')) + $separator
$probeIntermediateBase = [System.IO.Path]::GetFullPath((Join-Path $buildRoot 'probe\obj')) + $separator
$probeBuildArguments = @(
    Get-IsolatedBuildArguments `
        -Project $probeProject `
        -Configuration 'Debug' `
        -OutputBase $probeOutputBase `
        -IntermediateBase $probeIntermediateBase
)
& $dotnetExecutable @probeBuildArguments

if ($LASTEXITCODE -ne 0) {
    throw "Debugger probe build failed with exit code $LASTEXITCODE."
}

$probeAssembly = Join-Path $probeOutputBase 'Debug\net10.0\FSharp2.DebuggerProbe.dll'
$orderedInputPaths = @(
    (Join-Path $repositoryRoot 'global.json')
    (Join-Path $repositoryRoot 'Directory.Build.props')
    (Join-Path $repositoryRoot 'Directory.Packages.props')
    (Join-Path $repositoryRoot '.editorconfig')
    (Join-Path $repositoryRoot '.gitattributes')
    (Join-Path $repositoryRoot 'tests\Directory.Build.props')
    $fixtureProject
    (Join-Path $fixtureRoot 'packages.lock.json')
    $fixtureSource
    $probeProject
    (Join-Path $repositoryRoot 'tools\FSharp2.DebuggerProbe\packages.lock.json')
    (Join-Path $repositoryRoot 'tools\FSharp2.DebuggerProbe\DapSession.cs')
    (Join-Path $repositoryRoot 'tools\FSharp2.DebuggerProbe\Program.cs')
    $PSCommandPath
    $lockPath
    $noticeSourcePaths
)
$orderedInputs = @(
    for ($index = 0; $index -lt $orderedInputPaths.Count; $index++) {
        $identity = Get-Identity -Path $orderedInputPaths[$index] -RelativeTo $repositoryRoot
        [pscustomobject]@{
            ordinal = $index
            path = $identity.path
            length = $identity.length
            sha256 = $identity.sha256
        }
    }
)

$headLines = @(& git rev-parse HEAD 2>&1)
if ($LASTEXITCODE -ne 0 -or $headLines.Count -ne 1) {
    throw 'Unable to resolve the repository HEAD identity.'
}

$treeLines = @(& git rev-parse 'HEAD^{tree}' 2>&1)
if ($LASTEXITCODE -ne 0 -or $treeLines.Count -ne 1) {
    throw 'Unable to resolve the repository tree identity.'
}

$statusLines = @(& git status --short --untracked-files=all 2>&1)
if ($LASTEXITCODE -ne 0) {
    throw 'Unable to capture repository status for replay identity.'
}

$statusText = $statusLines -join "`n"
$bootstrapPath = Join-Path $evidenceRoot 'bootstrap.json'
$bootstrap = [ordered]@{
    schemaVersion = 1
    probeId = 'netcoredbg-focused-candidate-v1'
    finalConformanceClaimed = $false
    createdUtc = [System.DateTimeOffset]::UtcNow
    repository = [ordered]@{
        head = $headLines[0].Trim()
        headTree = $treeLines[0].Trim()
        statusSha256 = Get-Sha256Text -Text $statusText
        statusLines = $statusLines
    }
    protocol = $driverLock.protocol
    driver = [ordered]@{
        name = $driverLock.driver.name
        version = $driverLock.driver.version
        sourceCommit = $driverLock.driver.sourceCommit
        selectedAsset = $asset
        providedArchive = $providedArchiveIdentity
        materializedArchive = $archiveIdentity
        noticeSources = $noticeSourceIdentities
        materializedNotices = $noticeMirrorIdentities
        buildInfo = ($buildInfoLines -join [Environment]::NewLine)
        extractedFiles = $extractedFiles
    }
    oracleToolchain = [ordered]@{
        sdkVersion = $sdkVersion
        dotnetHost = $dotnetHostIdentity
        sdkFiles = $sdkFileIdentities
        dotnetInfo = ($dotnetInfoLines -join [Environment]::NewLine)
    }
    orderedInputs = $orderedInputs
    build = [ordered]@{
        fixtureArguments = $fixtureBuildArguments
        probeArguments = $probeBuildArguments
        mappedSource = Get-Identity -Path $mappedSource -RelativeTo $artifactsRoot
        program = Get-Identity -Path $program -RelativeTo $artifactsRoot
        pdb = Get-Identity -Path $pdb -RelativeTo $artifactsRoot
        probeAssembly = Get-Identity -Path $probeAssembly -RelativeTo $artifactsRoot
    }
}
$bootstrap | ConvertTo-Json -Depth 30 | Set-Content -LiteralPath $bootstrapPath -Encoding utf8
$bootstrapIdentity = Get-Identity -Path $bootstrapPath -RelativeTo $artifactsRoot

$attempts = @()

for ($attempt = 1; $attempt -le $AttemptCount; $attempt++) {
    $evidencePath = Join-Path $evidenceRoot "attempt-$attempt.json"

    & $dotnetExecutable $probeAssembly `
        --rid $rid `
        --archive $ArchivePath `
        --adapter $driverExecutable `
        --program $program `
        --pdb $pdb `
        --source $mappedSource `
        --bootstrap $bootstrapPath `
        --evidence $evidencePath

    $probeExitCode = $LASTEXITCODE

    if (-not (Test-Path -LiteralPath $evidencePath)) {
        throw "Debugger probe attempt $attempt produced no evidence."
    }

    $evidence = Get-Content -Raw -LiteralPath $evidencePath | ConvertFrom-Json
    $expectedExitCode = if ($evidence.driverProbeVerdict -eq 'pass') { 0 } else { 1 }

    if ($probeExitCode -ne $expectedExitCode) {
        throw "Debugger probe attempt $attempt exit code $probeExitCode disagrees with verdict '$($evidence.driverProbeVerdict)'."
    }

    $attempts += [pscustomobject]@{
        attempt = $attempt
        exitCode = $probeExitCode
        driverProbeVerdict = $evidence.driverProbeVerdict
        compilerVerdict = $evidence.compilerVerdict
        failureClass = $evidence.failureClass
        semanticFingerprint = $evidence.semanticFingerprint
        bootstrapSha256 = $evidence.bootstrap.sha256
        evidencePath = $evidencePath
    }
}

$semanticFingerprints = @($attempts.semanticFingerprint | Sort-Object -Unique)

if ($semanticFingerprints.Count -ne 1) {
    throw "The focused debugger candidate result was not repeatable across $AttemptCount attempts."
}

$actualVerdicts = @($attempts.driverProbeVerdict | Sort-Object -Unique)

if ($actualVerdicts.Count -ne 1) {
    throw "The focused debugger candidate produced inconsistent verdicts across $AttemptCount attempts."
}

$actualVerdict = $actualVerdicts[0]
$compilerVerdicts = @($attempts.compilerVerdict | Sort-Object -Unique)
$failureClasses = @($attempts | ForEach-Object { [string] $_.failureClass } | Sort-Object -Unique)
$bootstrapHashes = @($attempts.bootstrapSha256 | Sort-Object -Unique)
$summaryPath = Join-Path $evidenceRoot 'summary.json'
$summary = [pscustomobject]@{
    schemaVersion = 2
    probeId = 'netcoredbg-focused-candidate-v1'
    finalConformanceClaimed = $false
    rid = $rid
    protocol = $driverLock.protocol
    driver = [pscustomobject]@{
        name = $driverLock.driver.name
        version = $driverLock.driver.version
        sourceCommit = $driverLock.driver.sourceCommit
        asset = $asset
        buildInfo = ($buildInfoLines -join [Environment]::NewLine)
        extractedFiles = $extractedFiles
    }
    bootstrap = $bootstrapIdentity
    expected = $asset.focusedReplay
    actual = [pscustomobject]@{
        driverProbeVerdict = $actualVerdict
        compilerVerdicts = $compilerVerdicts
        failureClasses = $failureClasses
        semanticFingerprint = $semanticFingerprints[0]
        bootstrapSha256 = $bootstrapHashes
    }
    attempts = $attempts
}

$summary | ConvertTo-Json -Depth 30 | Set-Content -LiteralPath $summaryPath -Encoding utf8

if ($bootstrapHashes.Count -ne 1 -or $bootstrapHashes[0] -ne $bootstrapIdentity.sha256) {
    throw "Focused replay attempts were not bound to bootstrap '$($bootstrapIdentity.sha256)'. See $summaryPath."
}

if ($actualVerdict -ne [string] $asset.focusedReplay.driverProbeVerdict) {
    throw "Expected focused driver verdict '$($asset.focusedReplay.driverProbeVerdict)'; observed '$actualVerdict'. See $summaryPath."
}

if ($compilerVerdicts.Count -ne 1 -or
    $compilerVerdicts[0] -ne [string] $asset.focusedReplay.compilerVerdict) {
    throw "Focused replay compiler verdict did not match '$($asset.focusedReplay.compilerVerdict)'. See $summaryPath."
}

if ($failureClasses.Count -ne 1 -or
    $failureClasses[0] -ne [string] $asset.focusedReplay.failureClass) {
    throw "Focused replay failure class did not match '$($asset.focusedReplay.failureClass)'. See $summaryPath."
}

if ($semanticFingerprints[0] -ne [string] $asset.focusedReplay.semanticFingerprint) {
    throw "Focused replay semantic fingerprint did not match '$($asset.focusedReplay.semanticFingerprint)'. See $summaryPath."
}

Write-Output "Focused debugger candidate result '$actualVerdict' repeated $AttemptCount times for $rid."
Write-Output "This result does not claim final debugger conformance."
Write-Output "Summary: $summaryPath"
