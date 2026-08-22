module Program

let answer () = 42

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
