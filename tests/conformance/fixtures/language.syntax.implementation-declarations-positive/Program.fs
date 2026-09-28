namespace SyntaxDeclarations

open System

[<assembly: System.Reflection.AssemblyMetadata("SyntaxRow", "implementation-declarations")>]
do ()

module Values =
    let private seed = 40
    let internal add left right = left + right
    let answer () = add seed 2

module Program =
    [<EntryPoint>]
    let main _ = Values.answer () - 42
