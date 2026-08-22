module Program
let before value =
    match value with
    | Some item -> item
#nowarn "25"
let inside value =
    match value with
    | Some item -> item
#warnon "25"
let after value =
    match value with
    | Some item -> item
let answer () = after (Some 42)
[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
