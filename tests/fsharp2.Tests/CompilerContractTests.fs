namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Reflection
open Expecto

module CompilerContractTests =
    type private InvocationResult = {
        ExitCode: int
        StandardOutput: string
        StandardError: string
    }

    let private repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let private configuration =
        let releaseSegment =
            $"{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}"

        if
            AppContext.BaseDirectory.Contains(releaseSegment, StringComparison.OrdinalIgnoreCase)
        then
            "Release"
        else
            "Debug"

    let private invokeProcess
        (workingDirectory: string)
        (timeoutMilliseconds: int)
        (fileName: string)
        (arguments: string list)
        =
        let startInfo = ProcessStartInfo(fileName)
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEndAsync()
        let standardError = child.StandardError.ReadToEndAsync()

        if not (child.WaitForExit(timeoutMilliseconds)) then
            child.Kill(true)
            failtestf "%s did not exit within %d milliseconds" fileName timeoutMilliseconds

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput.Result
            StandardError = standardError.Result
        }

    let private compatibilityOraclePath () =
        let versionResult = invokeProcess repositoryRoot 10_000 "dotnet" [ "--version" ]

        Expect.equal
            versionResult.ExitCode
            0
            (versionResult.StandardOutput
             + versionResult.StandardError)

        let version = versionResult.StandardOutput.Trim()
        let sdkList = invokeProcess repositoryRoot 10_000 "dotnet" [ "--list-sdks" ]

        Expect.equal
            sdkList.ExitCode
            0
            (sdkList.StandardOutput
             + sdkList.StandardError)

        let prefix =
            version
            + " ["

        let sdkRoot =
            sdkList.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun line ->
                let line = line.Trim()

                if
                    line.StartsWith(prefix, StringComparison.Ordinal)
                    && line.EndsWith(']')
                then
                    Some(
                        line.Substring(
                            prefix.Length,
                            line.Length
                            - prefix.Length
                            - 1
                        )
                    )
                else
                    None
            )
            |> Option.defaultWith (fun () ->
                failtestf "the active .NET SDK '%s' was not present in dotnet --list-sdks" version
            )

        Path.Combine(sdkRoot, version, "FSharp", "fsc.dll")

    let private compileWithOracle root responseName outputPath sourcePath =
        let responsePath = Path.Combine(root, responseName)

        File.WriteAllLines(
            responsePath,
            [|
                "--target:library"
                "--targetprofile:netcore"
                "--deterministic+"
                "--debug:portable"
                "--optimize-"
                $"--out:{outputPath}"
                sourcePath
            |]
        )

        invokeProcess root 30_000 "dotnet" [
            compatibilityOraclePath ()
            "@"
            + responsePath
        ]

    let private compileWithFSharp2 root outputPath sourcePath references additionalOptions =
        let responsePath = Path.Combine(root, "fsharp2.rsp")
        let systemRuntimePath = Assembly.Load("System.Runtime").Location

        let arguments =
            [
                [
                    "--target:library"
                    "--deterministic+"
                    "--debug:portable"
                    $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                    $"--reference:{systemRuntimePath}"
                ]
                references
                |> List.map (fun referencePath -> $"--reference:{referencePath}")
                additionalOptions
                [
                    $"--out:{outputPath}"
                    sourcePath
                ]
            ]
            |> List.concat

        File.WriteAllLines(responsePath, arguments)

        let compilerArguments = [
            "run"
            "--no-build"
            "--configuration"
            configuration
            "--project"
            $"{repositoryRoot}/src/fsc2.Prototype/fsc2.Prototype.fsproj"
            "--"
            "@"
            + responsePath
        ]

        invokeProcess root 30_000 "dotnet" compilerArguments

    [<Tests>]
    let tests =
        testList "Compiler Contract" [
            testCase "corpus neutral value-task builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-value-task-builder",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

open System.Threading.Tasks

type WorkflowBuilder() =
    member _.Bind(source: ValueTask<'T>, continuation: 'T -> ValueTask<'U>) : ValueTask<'U> =
        ValueTask<'U>(task {
            let! value = source.AsTask()
            return! (continuation value).AsTask()
        })

    member _.ReturnFrom(source: ValueTask<'T>) = source
    member _.Delay(generator: unit -> ValueTask<'T>) = generator
    member _.Run(generator: unit -> ValueTask<'T>) = generator()

[<AutoOpen>]
module Builders =
    let workflow = WorkflowBuilder()
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "ValueTaskCase.fs")
                    let outputPath = Path.Combine(root, "ValueTaskCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module ValueTaskCase =
    let inline bind
        ([<InlineIfLambda>] (binder: 'input -> ValueTask<'output>))
        (source: ValueTask<'input>)
        =
        workflow {
            let! value = source
            return! binder value
        }
"""
                    )

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral unit-task parallel builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-unit-task-parallel",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

open System.Threading.Tasks

type WorkflowBuilder() =
    member _.Bind(source: Task<'T>, continuation: 'T -> Task<'U>) : Task<'U> =
        task {
            let! value = source
            return! continuation value
        }

    member _.Return(value: 'T) = Task.FromResult(value)
    member _.Delay(generator: unit -> Task<'T>) = generator
    member _.Run(generator: unit -> Task<'T>) = generator

type WorkflowHelpers =
    static member GetAwaiter(computation: 'T) : 'T = computation

[<AutoOpen>]
module Builders =
    let workflow = WorkflowBuilder()
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "ParallelCase.fs")
                    let outputPath = Path.Combine(root, "ParallelCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module ParallelCase =
    let inline zip
        (left: unit -> Task<'left>)
        (right: unit -> Task<'right>)
        =
        workflow {
            let leftTask = left ()
            let rightTask = right ()
            let! leftResult = leftTask
            let! rightResult = rightTask
            return leftResult, rightResult
        }
        |> WorkflowHelpers.GetAwaiter
"""
                    )

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral environment parallel builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-environment-parallel",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

open System.Threading
open System.Threading.Tasks

type WorkflowBuilder() =
    member _.Bind(
        source: CancellationToken -> ValueTask<'T>,
        continuation: 'T -> CancellationToken -> ValueTask<'U>
    ) : CancellationToken -> ValueTask<'U> =
        fun cancellationToken ->
            ValueTask<'U>(task {
                let! value = (source cancellationToken).AsTask()
                return! (continuation value cancellationToken).AsTask()
            })

    member _.Bind(
        source: ValueTask<'T>,
        continuation: 'T -> CancellationToken -> ValueTask<'U>
    ) : CancellationToken -> ValueTask<'U> =
        fun cancellationToken ->
            ValueTask<'U>(task {
                let! value = source.AsTask()
                return! (continuation value cancellationToken).AsTask()
            })

    member _.Return(value: 'T) : CancellationToken -> ValueTask<'T> =
        fun _ -> ValueTask<'T>(value)

    member _.Delay(generator: unit -> CancellationToken -> ValueTask<'T>) = generator

    member _.Run(generator: unit -> CancellationToken -> ValueTask<'T>) = generator()

[<AutoOpen>]
module Builders =
    let workflow = WorkflowBuilder()
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "ParallelCase.fs")
                    let outputPath = Path.Combine(root, "ParallelCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open System.Threading
open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module ParallelCase =
    let inline zip
        (currentContext: unit -> CancellationToken -> ValueTask<CancellationToken>)
        (left: CancellationToken -> ValueTask<'left>)
        (right: CancellationToken -> ValueTask<'right>)
        =
        workflow {
            let! cancellationToken = currentContext ()
            let leftTask = left cancellationToken
            let rightTask = right cancellationToken
            let! leftResult = leftTask
            let! rightResult = rightTask
            return leftResult, rightResult
        }
"""
                    )

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral cancellation-token task sequence builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-task-sequence",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

open System.Threading
open System.Threading.Tasks

type WorkflowBuilder() =
    member _.Delay(generator: unit -> (CancellationToken -> Task<'T>)) =
        fun cancellationToken -> generator () cancellationToken

    member _.Zero() : CancellationToken -> Task<unit> =
        fun _ -> Task.FromResult(())

    member _.Return(value: 'T) : CancellationToken -> Task<'T> =
        fun _ -> Task.FromResult(value)

    member _.Bind(
        source: CancellationToken -> Task<'T>,
        continuation: 'T -> CancellationToken -> Task<'U>
    ) : CancellationToken -> Task<'U> =
        fun cancellationToken ->
            task {
                let! value = source cancellationToken
                return! continuation value cancellationToken
            }

    member _.Combine(
        first: CancellationToken -> Task<unit>,
        second: CancellationToken -> Task<'T>
    ) : CancellationToken -> Task<'T> =
        fun cancellationToken ->
            task {
                do! first cancellationToken
                return! second cancellationToken
            }

    member _.For(
        sequence: seq<'T>,
        body: 'T -> CancellationToken -> Task<unit>
    ) : CancellationToken -> Task<unit> =
        fun cancellationToken ->
            task {
                for item in sequence do
                    do! body item cancellationToken
            }

    member _.Run(computation: CancellationToken -> Task<'T>) = computation

type ResultBuffer<'T>() =
    let values = ResizeArray<'T>()

    member _.Store(value: 'T) = values.Add(value)
    member _.Finish() = values.ToArray()

[<AutoOpen>]
module Builders =
    let workflow = WorkflowBuilder()
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "SequenceCase.fs")
                    let outputPath = Path.Combine(root, "SequenceCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module SequenceCase =
    let inline sequence<'T>
        (tasks: IEnumerable<CancellationToken -> Task<'T>>)
        =
        workflow {
            let results = ResultBuffer<'T>()

            for taskFactory in tasks do
                let! result = taskFactory
                results.Store result

            return results.Finish()
        }
"""
                    )

                    let systemCollectionsPath = Assembly.Load("System.Collections").Location

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [
                            systemCollectionsPath
                            builderOutputPath
                        ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral resumable-code diagnostic"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-resumable-diagnostic",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "ResumableCase.fs")
                    let outputPath = Path.Combine(root, "ResumableCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open Microsoft.FSharp.Core.CompilerServices

[<AutoOpen>]
module ResumableCase =
    [<Struct; NoComparison; NoEquality>]
    type Data<'T> =
        [<DefaultValue(false)>]
        val mutable Result: 'T

    type Code<'T> = ResumableCode<Data<'T>, 'T>

    type Builder() =
        member inline _.Return(value: 'T) : Code<'T> =
            Code<'T>(fun _ -> false)
"""
                    )

                    let result = compileWithFSharp2 root outputPath sourcePath [] []

                    let diagnostics =
                        result.StandardOutput
                        + result.StandardError

                    Expect.equal result.ExitCode 1 diagnostics

                    Expect.stringContains
                        diagnostics
                        "the resumable code lambda shape is not supported"
                        "the diagnostic should describe the unsupported compiler semantic"

                    Expect.isFalse
                        (diagnostics.Contains("IcedTasks", StringComparison.Ordinal))
                        "the diagnostic should not name a compatibility corpus"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral value-task apply zip and unit builders"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-value-task-shapes",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

open System.Threading.Tasks

type WorkflowBuilder() =
    member _.Bind(source: ValueTask<'T>, continuation: 'T -> ValueTask<'U>) : ValueTask<'U> =
        ValueTask<'U>(task {
            let! value = source.AsTask()
            return! (continuation value).AsTask()
        })

    member _.Return(value: 'T) = ValueTask<'T>(value)
    member _.ReturnFrom(source: ValueTask<'T>) = source

    member _.ReturnFrom(source: ValueTask) : ValueTask<unit> =
        ValueTask<unit>(task {
            do! source.AsTask()
            return ()
        })

    member _.Delay(generator: unit -> ValueTask<'T>) = generator
    member _.Run(generator: unit -> ValueTask<'T>) = generator()

[<AutoOpen>]
module Builders =
    let workflow = WorkflowBuilder()
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let cases = [
                        "Apply",
                        """namespace ContractCases

open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module ApplyCase =
    let inline apply
        (applicable: ValueTask<'input -> 'output>)
        (source: ValueTask<'input>)
        =
        workflow {
            let! applier = applicable
            let! value = source
            return applier value
        }
"""
                        "Zip",
                        """namespace ContractCases

open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module ZipCase =
    let inline zip (left: ValueTask<'left>) (right: ValueTask<'right>) =
        workflow {
            let! leftResult = left
            let! rightResult = right
            return leftResult, rightResult
        }
"""
                        "Unit",
                        """namespace ContractCases

open System.Threading.Tasks
open ContractBuilders

[<RequireQualifiedAccess>]
module UnitCase =
    let inline ofUnit (source: ValueTask) : ValueTask<unit> =
        if source.IsCompletedSuccessfully then
            ValueTask<unit>()
        else
            workflow { return! source }
"""
                    ]

                    let failures =
                        cases
                        |> List.choose (fun (caseName, sourceText) ->
                            let sourcePath =
                                Path.Combine(
                                    root,
                                    caseName
                                    + ".fs"
                                )

                            let outputPath =
                                Path.Combine(
                                    root,
                                    caseName
                                    + ".dll"
                                )

                            File.WriteAllText(sourcePath, sourceText)

                            let result =
                                compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                            if
                                result.ExitCode = 0
                                && File.Exists(outputPath)
                            then
                                None
                            else
                                Some(
                                    caseName
                                    + Environment.NewLine
                                    + result.StandardOutput
                                    + result.StandardError
                                )
                        )

                    Expect.isEmpty failures (String.concat Environment.NewLine failures)
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral task try-finally builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-task-try-finally",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

[<AutoOpen>]
module Builders =
    let workflow = task
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "TryFinallyCase.fs")
                    let outputPath = Path.Combine(root, "TryFinallyCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open System
open System.Threading.Tasks
open ContractBuilders

type TryFinallyCase() =
    static member inline Run(wait: Task, work: Task<int>, release: Action) : Task<int> =
        workflow {
            do! wait

            try
                return! work
            finally
                release.Invoke()
        }
"""
                    )

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "corpus neutral async while builder"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-async-while",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let builderSourcePath = Path.Combine(root, "Builder.fs")
                    let builderOutputPath = Path.Combine(root, "Builder.dll")

                    File.WriteAllText(
                        builderSourcePath,
                        """namespace ContractBuilders

[<AutoOpen>]
module Builders =
    let workflow = async
"""
                    )

                    let builderResult =
                        compileWithOracle root "builder.rsp" builderOutputPath builderSourcePath

                    Expect.equal
                        builderResult.ExitCode
                        0
                        (builderResult.StandardOutput
                         + builderResult.StandardError)

                    let sourcePath = Path.Combine(root, "AsyncWhileCase.fs")
                    let outputPath = Path.Combine(root, "AsyncWhileCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

open ContractBuilders

type AsyncWhileCase() =
    member inline _.Run(guard: Async<bool>, computation: Async<unit>) =
        workflow {
            let mutable keepGoing = true

            while keepGoing do
                let! guardResult = guard
                if guardResult then do! computation else keepGoing <- false
        }
"""
                    )

                    let result =
                        compileWithFSharp2 root outputPath sourcePath [ builderOutputPath ] []

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    Expect.isTrue
                        (File.Exists(outputPath))
                        "the compiler should emit the output assembly"
                finally
                    Directory.Delete(root, true)

            testCase "tailcalls minus rejects an unsupported option"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-contract-tailcalls-minus",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TailcallsCase.fs")
                    let outputPath = Path.Combine(root, "TailcallsCase.dll")

                    File.WriteAllText(
                        sourcePath,
                        """namespace ContractCases

type TailcallsCase() =
    member inline _.Value() = 42
"""
                    )

                    let result = compileWithFSharp2 root outputPath sourcePath [] [ "--tailcalls-" ]

                    let diagnostics =
                        result.StandardOutput
                        + result.StandardError

                    Expect.equal result.ExitCode 1 diagnostics

                    Expect.stringContains
                        diagnostics
                        "unsupported prototype option: --tailcalls-"
                        "the compiler should reject an option whose semantics are not implemented"

                    Expect.isFalse
                        (File.Exists(outputPath))
                        "the compiler should not emit an output assembly"
                finally
                    Directory.Delete(root, true)
        ]
