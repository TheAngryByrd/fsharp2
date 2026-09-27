module Program

let before (value: int option) =
    match value with
    | Some item -> item

#nowarn "25"

let inside (value: int option) =
    match value with
    | Some item -> item

#warnon "25"

let after (value: int option) =
    match value with
    | Some item -> item

let answer () = 42

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
