module FSharp2.DebuggerFixture

open System
open System.Threading.Tasks

type Summary = { Input: int; Doubled: int }

let increment value =
    let incremented = value + 1
    incremented

let ordinary input =
    let doubled = input * 2
    let summary = { Input = input; Doubled = doubled }
    let result = increment summary.Doubled // BREAKPOINT:ordinary
    result

let asynchronous input =
    task {
        let beforeYield = ordinary input
        do! Task.Yield()

        let afterYield =
            beforeYield
            + 1 // BREAKPOINT:task

        return afterYield
    }

[<EntryPoint>]
let main _ =
    let result =
        asynchronous 20
        |> fun work -> work.GetAwaiter().GetResult()

    Console.WriteLine(result)

    if result = 42 then 0 else 1
