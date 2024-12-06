module TestUtils

open Xunit
let inline (==>) x y = (x, y)
let inline checkExamples<'a when 'a : equality> (examples: (bigint * 'a) list ) f = 
    examples |> List.forall(fun (i, e) -> Assert.Equal(e, f i); true)

let inline checkMapping<'a, 'b when 'a : equality and 'b : equality> f (examples: ('a * 'b) list) = 
    examples |> List.forall(fun (a, b) -> Assert.Equal((f a), b); true)

let inline assertEqual<'a when 'a : equality> (actual: 'a) (expected: 'a) = 
    Assert.Equal(expected, actual)
