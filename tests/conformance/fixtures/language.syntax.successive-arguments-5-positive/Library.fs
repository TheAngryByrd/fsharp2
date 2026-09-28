module Library

let g (x: int) = x

let h (x: int) (ys: int list) = x

let one (xs: int list) = 5

let a = g one[0]

let b = h one[0][1]
