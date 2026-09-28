namespace SyntaxSignatures

module Values =
    val answer: int
    val add: left: int -> right: int -> int
    val pair: int * string
    val map: ('a -> 'b) -> 'a list -> list<'b>
