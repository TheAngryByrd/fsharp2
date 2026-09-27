module Program

// lexical forms
let answer () = 42

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
