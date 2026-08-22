module Program

#if TRACE
let answer () = 42
#elif OTHER
let answer () = )
#else
let answer () = )
#endif

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
