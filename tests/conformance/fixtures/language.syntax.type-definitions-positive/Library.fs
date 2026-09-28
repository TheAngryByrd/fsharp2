module Shapes

type Point = { X: int; mutable Y: int }

type Shape =
    | Circle of radius: float
    | Square of float
    | Empty

type Counter(start: int) =
    member this.Start = start
    member _.Add(x: int, y: int) = x + y + start
    static member Zero = Counter(0)

type Alias = int list
