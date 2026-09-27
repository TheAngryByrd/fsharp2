module Program

#if TRACE
let answer () = 42
#else
#if OTHER
let answer () = )
#else
let answer () = )
#endif
#endif

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
