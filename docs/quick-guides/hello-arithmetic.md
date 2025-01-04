## Hello Arithmetic!

Let's get started by downloading the toolkit and opening a new F# script file.

```fsharp
//  Load the toolkit
#load "godelian-toolkit.fsx"
open GodelianTooklit
```

We'll define a simple expression type that can represent numbers, negation, addition, and multiplication.

```fsharp
//  Define your expression type
type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr
```

We're now going to create what we like to call a **Gödelian constructor**.  A Gödelian constructor is a function that takes an arbitrarily large integer (bigint) and returns a unique instance of your expression type.

```fsharp
//  Here's our Gödelian constructor for Expr
let chooseExpr: bigint -> Expr =
    combineChoices
        [   fun enc n -> Num(n)
            fun enc n -> Neg(enc n)
            fun enc (Pair(l, r)) -> Add(enc l, enc r)
            fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]
```

That's it!

```fsharp


//  Try printing out the first 10 instances!
for i in 0I .. 10I do
    let e = chooseExpr i
    printfn "Godel Number: %A = %s" i (toString e)

//  You can also get the value of any single expression
//  at any time
printfn "%A" (chooseExpr 12398723987123I)

// You may want an evaluator
let rec eval =
    function
    | Num n -> n
    | Neg e -> -(eval e)
    | Add(l, r) -> eval l + eval r
    | Mul(l, r) -> eval l * eval r

//  Find all the expressions that evaluate to 42 in the first 1000 instances
for i in 0I .. 1000I do
    let e = chooseExpr i
    if eval e = 42I then
        printfn "Godel Number: %A = %s" i (toString e)
```

Let's try a slightly more interesting language.