////////////////////////////////////////////////////////////////////////
///  HOW TO USE IT
////////////////////////////////////////////////////////////////////////

#load "godelian-toolkit.fsx"

open GodelianTooklit

// Define a recursive type
type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr

// Create your universal constructor!
let chooseExpr: bigint -> Expr =
    combineChoices
        [   fun enc n -> Num(n)
            fun enc n -> Neg(enc n)
            fun enc (Pair(l, r)) -> Add(enc l, enc r)
            fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]

// May want to make them look pretty
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

//  You may want to search for expressions that evaluate to 42
//  in a particular range.  You can start anywhere, but here
//  we start at 0 and go up to 9999999999999999
let expressionsThatEqual42 =
    seq {
        for i in 0I .. 9999999999999999I do
            let e = chooseExpr i
            if eval e = 42I then
                yield i, e
    }

//  Let's only print the first 1000
for (i, e) in expressionsThatEqual42 |> Seq.truncate 1000 do
    let e = chooseExpr i
    printfn "%A -> %s" i (toString e)

// Example output
// 168 -> 42
// 2693 -> -(-(42))
// 3235 -> (6 * 7)
// 3267 -> (7 * 6)
// 12595 -> (3 * 14)
// 12947 -> (14 * 3)
// 28259 -> (2 * 21)
// 28555 -> ((1 + 1) * 21)
// 28562 -> (21 + 21)
// 28571 -> (21 * (1 + 1))
// 28867 -> (21 * 2)
// 31298 -> (20 + 22)
// ...