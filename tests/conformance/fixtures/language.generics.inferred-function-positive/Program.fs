module Program

let identity value = value

[<EntryPoint>]
let main _ =
    let number = identity 42
    let text = identity "forty-two"
    printfn "%d,%s" number text
    0
