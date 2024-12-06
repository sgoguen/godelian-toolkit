module MathExample

open GodelianTooklit

type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr

// Pretty print
let rec toString =
    function
    | Num n -> n.ToString()
    | Neg e -> sprintf "-(%s)" (toString e)
    | Add(l, r) -> sprintf "(%s + %s)" (toString l) (toString r)
    | Mul(l, r) -> sprintf "(%s * %s)" (toString l) (toString r)

// You may want an evaluator
let rec eval =
    function
    | Num n -> n
    | Neg e -> -(eval e)
    | Add(l, r) -> eval l + eval r
    | Mul(l, r) -> eval l * eval r

let createSimple: bigint -> Expr =
    combineChoices
        [ fun enc n -> Num(n)
          fun enc n -> Neg(enc n)
          fun enc (Pair(l, r)) -> Add(enc l, enc r)
          fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]

open Xunit

let inline (==>) x y = (x, y)

let examples = [
    0I ==> Num(0I)
    1I ==> Neg(Num(0I))
    2I ==> Add(Num(0I), Num(0I))
]

[<Fact>]
let checkExamples() = 
    for (godelNum, e) in examples do
        let actual = createSimple godelNum
        Assert.Equal(e, actual)
    Assert.True(true)