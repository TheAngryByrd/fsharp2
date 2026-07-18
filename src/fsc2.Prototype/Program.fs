namespace FSharp2.Compiler

module Program =
    [<EntryPoint>]
    let main arguments = CompilerHost.run arguments
