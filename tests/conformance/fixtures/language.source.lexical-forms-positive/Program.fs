module Program

// nested lexical forms
(* outer (* inner *) outer *)
let text = @"raw"
let number = 0x2A
let answer () = number

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
