////////////////////////////////////////////////////////////////////////
///  HOW TO USE IT
////////////////////////////////////////////////////////////////////////

#load "godelian-toolkit.fsx"

open GodelianTooklit

// Define a recursive type
type Expr =
    | Const of int
    | Var of string
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr

let rec toString =
    function
    | Const n -> n.ToString()
    | Var n -> n
    | Neg e -> sprintf "-(%s)" (toString e)
    | Add(l, r) -> sprintf "(%s + %s)" (toString l) (toString r)
    | Mul(l, r) -> sprintf "(%s * %s)" (toString l) (toString r)

let getName n = Strings.Alpha.fromInt n

type Variables(varsAvailable: bigint) =
    member this.Count = int (varsAvailable)
    member this.Pick(i) = getName (i)

    member this.NewVar() =
        let name = this.Pick(varsAvailable)
        (name, Variables(varsAvailable + 1I))

// Create your universal constructor!
let rec chooseExpr: bigint -> Expr =
    combineChoices
        [   fun enc n -> Const(int(n))
            fun enc n -> Var(getName (n))
            fun enc n -> Neg(enc n)
            fun enc (Pair(l, r)) -> Add(enc l, enc r)
            fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]
and (|E|) = chooseExpr

// let createClosedTerm: bigint -> Expr =
//     let initialVariables = Variables(0I)

//     initialVariables
//     |> combineChoicesWithContext (fun (vars) ->
//         tryFiniteFirst
//             vars.Count
//             (fun i -> Var(vars.Pick(i)))
//             [ fun enc n ->
//                   let (name, newVars) = vars.NewVar()
//                   Lamda(name, enc newVars n)
//               fun enc (Pair(l, r)) -> App(enc vars l, enc vars r) ])            

//  Try printing out the first 10 instances!
for i in 0I .. 10I do
    let e = chooseExpr i
    printfn "Godel Number: %A = %A" i (toString e)

// Godel Number: 0 = Num 0
// Godel Number: 1 = Neg (Num 0)
// Godel Number: 2 = Add (Num 0, Num 0)
// Godel Number: 3 = Mul (Num 0, Num 0)
// Godel Number: 4 = Num 1
// Godel Number: 5 = Neg (Neg (Num 0))
// Godel Number: 6 = Add (Num 0, Neg (Num 0))
// Godel Number: 7 = Mul (Num 0, Neg (Num 0))
// Godel Number: 8 = Num 2
// Godel Number: 9 = Neg (Add (Num 0, Num 0))
// Godel Number: 10 = Add (Neg (Num 0), Neg (Num 0))

//  You can also get the value of any single expression
//  at any time
printfn "%A" (chooseExpr 12398723987123I)

// This will print:
// Mul
//   (Mul (Add (Mul (Num 0, Num 0), Num 1), Add (Mul (Num 0, Neg (Num 0)), Num 2)),
//    Mul (Add (Num 0, Num 3), Mul (Num 3, Mul (Num 0, Num 0))))

// May want your expressions to look pretty
let rec toString =
    function
    | Num n -> n.ToString()
    | Neg e -> sprintf "-(%s)" (toString e)
    | Add(l, r) -> sprintf "(%s + %s)" (toString l) (toString r)
    | Mul(l, r) -> sprintf "(%s * %s)" (toString l) (toString r)


//  Let's sample the first 10 expressions again
for i in 0I .. 10I do
    let e = chooseExpr i
    printfn "Godel Number: %A = %s" i (toString e)

// Godel Number: 0 = 0
// Godel Number: 1 = -(0)
// Godel Number: 2 = (0 + 0)
// Godel Number: 3 = (0 * 0)
// Godel Number: 4 = 1
// Godel Number: 5 = -(-(0))
// Godel Number: 6 = (0 + -(0))
// Godel Number: 7 = (0 * -(0))
// Godel Number: 8 = 2
// Godel Number: 9 = -((0 + 0))
// Godel Number: 10 = (-(0) + -(0))

// You may want an evaluator
let rec eval =
    function
    | Num n -> n
    | Neg e -> -(eval e)
    | Add(l, r) -> eval l + eval r
    | Mul(l, r) -> eval l * eval r

//  Let's find all the expressions that evaluate to 42
for i in 0I .. 20000I do
    let e = chooseExpr i
    if eval e = 42I then
        printfn "Godel Number: %A = %s" i (toString e)

// Godel Number: 168 = 42
// Godel Number: 2693 = -(-(42))
// Godel Number: 3235 = (6 * 7)
// Godel Number: 3267 = (7 * 6)
// Godel Number: 12595 = (3 * 14)
// Godel Number: 12947 = (14 * 3)        