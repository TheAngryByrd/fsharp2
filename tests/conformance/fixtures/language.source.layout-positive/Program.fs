module Program

let answer () =
    let value = 40
    value + 2

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
