module Expressions

type Point = { X: int; Y: int }

let classify value =
    match value with
    | Some 0 -> "zero"
    | Some n when n > 0 -> "positive"
    | Some _ -> "negative"
    | None -> "none"

let pick flag = if flag then 1 else 2

let choose flag =
    if flag then
        [ 1; 2; 3 ]
    else
        []

let increment = fun x -> x + 1

let apply = List.map (fun (x: int) y -> x + y) [ 1 ]

let origin = { X = 0; Y = 0 }

let items = [ origin.X; 1 ]
