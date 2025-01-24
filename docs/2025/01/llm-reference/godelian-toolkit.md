# An LLM's Guide to the Godelian Toolkit

## Overview

The Gödelian Toolkit is a set of F# functions and types that facilitate constructing bijective (one-to-one) mappings between bigint values and instances of inductive data types (e.g., ASTs for programming languages).

This toolkit allows you to create what we want to call Bijective Gödelian constructors, which is a function that takes a bigint and returns an instance of some data type.  A Bijective Gödelian constructors should always assign a unique bigint to each unique instance (value-wise comparison, not reference-wise comparison)
so that it effectively indexes the set of all possible **VALID** instances of the data type that you want to construct.

## The Source Code for godelian-toolkit.fs

```fsharp
////////////////////////////////////////////////////////////////////////
///  The Gödelian Toolkit
////////////////////////////////////////////////////////////////////////

module GodelianTooklit

let sqrt (z: bigint) : bigint =
    if z < 0I then
        invalidArg "z" "Cannot compute the square root of a negative number"
    elif z = 0I then
        0I
    else
        let rec newtonRaphson (x: bigint) : bigint =
            let nextX = (x + z / x) / 2I
            if nextX >= x then x else newtonRaphson nextX

        newtonRaphson z

let encodePair (z: bigint) : bigint * bigint =
    let m = sqrt (z)
    let m2 = m * m
    if z - m2 < m then (z - m2, m) else (m, m2 + 2I * m - z)

let (|Pair|) = encodePair

let decodePair (p: bigint * bigint) : bigint =
    let (x, y) = p
    let m = max x y
    m * m + m + x - y


let combineChoices (functionList: ((bigint -> 'a) -> bigint -> 'a) list) (n: bigint): 'a =
    let length = bigint (List.length functionList)

    let rec chooseFunction n =
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction n


let combineChoicesWithContext (getOptions: 'a -> (('a -> bigint -> 'b) -> bigint -> 'b) list) (initialContext: 'a): bigint -> 'b =

    //  We add a parameter that now includes context
    let rec chooseFunction context n =
        let functionList = getOptions (context)
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction initialContext


let tryFiniteFirst (numberOfFiniteOptions: int) (finiteConstuctor: int -> 'b) (infiniteConstructors: ('a -> bigint -> 'b) list) =
    let numberOfFiniteOptions = numberOfFiniteOptions - 1
    let length = bigint (List.length infiniteConstructors)

    if numberOfFiniteOptions >= 0 then
        [ (fun enc n ->
              if n <= (bigint numberOfFiniteOptions) then
                  finiteConstuctor (int n)
              else
                  let n = n - (bigint (numberOfFiniteOptions + 1))
                  let (d, r) = bigint.DivRem(n, length)
                  let f = infiniteConstructors.[int r]
                  f enc d) ]
    else
        infiniteConstructors
```

## An Example of Using the Toolkit

```fsharp
#load "godelian-toolkit.fsx"

open GodelianTooklit

// Define a simple Lambda Calculus language
// with a simple default syntax
type Term =
    | Var of string // Variable reference
    | Lamda of string * Term // Abstraction - (Lambas)
    | App of Term * Term // Function application

    override this.ToString() =
        match this with
        | Var name -> name
        | Lamda(name, body) -> sprintf "λ%s.%s" name (body.ToString())
        | App(l, r) -> sprintf "(%s %s)" (l.ToString()) (r.ToString())


// Let's use this utility from the toolkit to turn numbers into strings
// for our variable names.
let getName n = Strings.Alpha.fromInt n

//  We can create a naive constructor that generates all terms
//  Unfortunately, this will generate terms that are not closed.
//  This means it will define functions that reference variables
//  that are not bound by a lambda function.  :'(
let naiveConstructor: bigint -> Term =
    combineChoices
        [ fun enc varName -> Var(getName varName)
          fun enc (Pair(name, body)) -> Lamda(getName name, enc body)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]

//  Let's take a look at some examples
// for i in 0I .. 10I do
//     let e = naiveConstructor i
//     printfn "Godel Number: %A = %A" i (e.ToString())

// Produces examples like:
// Godel Number: 0 = "a"
// Godel Number: 1 = "λa.a"
// Godel Number: 2 = "(a a)"
// Godel Number: 3 = "b"
// Godel Number: 4 = "λa.λa.a"
// Godel Number: 5 = "(a λa.a)"
// Godel Number: 6 = "c"
// Godel Number: 7 = "λb.λa.a"
// Godel Number: 8 = "(λa.a λa.a)"
// Godel Number: 9 = "d"
// Godel Number: 10 = "λb.a"

//  Unfortunately, this will generate terms that are not closed.

//  First, let's define a function that checks if a term is closed.

let isClosed (t: Term) =
    let rec isClosedRec (t: Term) (variables) =
        match t with
        | Var name -> Set.contains name variables
        | Lamda(varName, t) -> isClosedRec t (Set.add varName variables)
        | App(l, r) -> isClosedRec l variables && isClosedRec r variables

    isClosedRec t Set.empty

//  Let's test it out
// for i in 0I .. 50I do
//     let e = naiveConstructor i
//     if isClosed e then
//         printfn "Godel Number: %A = %A" i (e.ToString())

// Godel Number: 1 = "λa.a"
// Godel Number: 4 = "λa.λa.a"
// Godel Number: 7 = "λb.λa.a"
// Godel Number: 8 = "(λa.a λa.a)"
// Godel Number: 13 = "λa.(a a)"
// Godel Number: 22 = "λc.λa.a"
// Godel Number: 31 = "λb.b"
// Godel Number: 43 = "λd.λa.a"
// Godel Number: 49 = "λa.λa.λa.a"

//  This looks better, but it's worth pointing out that #1 and #31 are
//  the same term.  While the names are different, the terms are the same.
//  This is called alpha equivalence.


//  We can fix this by using some new tools in the toolkit.
//  1. While we're constructing an instance of our inductive type,
//     we add a notion of context that we can keep track of while
//     we're constructing the term.  In this case, we'll keep track
//     of the number of variables that are available to bind.
//  2. We also need to treat finite and infinite options differently.
//     Here we use tryFiniteFirst to map the first n options to some
//     finite constructor, everything else is mapped to an infinite
//     constructor.

type Variables(varsAvailable: bigint) =
    member this.Count = int (varsAvailable)
    member this.Pick(i) = getName (i)

    member this.NewVar() =
        let name = this.Pick(varsAvailable)
        (name, Variables(varsAvailable + 1I))

let createClosedTerm: bigint -> Term =
    let initialVariables = Variables(0I)

    initialVariables
    |> combineChoicesWithContext (fun (vars) ->
        tryFiniteFirst
            vars.Count
            (fun i -> Var(vars.Pick(i)))
            [ fun enc n ->
                  let (name, newVars) = vars.NewVar()
                  Lamda(name, enc newVars n)
              fun enc (Pair(l, r)) -> App(enc vars l, enc vars r) ])

// #time "on"

for i in 0I .. 10I do
    let e = createClosedTerm i
    printfn "Godel Number: %A = %A" i (e.ToString())

// Produces examples like:

// Godel Number: 0 = "λa.a"
// Godel Number: 1 = "(λa.a λa.a)"
// Godel Number: 2 = "λa.λb.a"
// Godel Number: 3 = "(λa.a (λa.a λa.a))"
// Godel Number: 4 = "λa.(a a)"
// Godel Number: 5 = "((λa.a λa.a) (λa.a λa.a))"
// Godel Number: 6 = "λa.λb.b"
// Godel Number: 7 = "((λa.a λa.a) λa.a)"
// Godel Number: 8 = "λa.(a λb.a)"
// Godel Number: 9 = "(λa.a λa.λb.a)"
// Godel Number: 10 = "λa.λb.λc.a"
// Godel Number: 11 = "((λa.a λa.a) λa.λb.a)"


```

I'm trying to think of different examples other than the one above.   In this examples I would love for us to show a regular programmer who doesn't know lambda calculus how they can use this Godelian Toolkit to solve problems by constructing bijections and then enumerating over those bijections.  I would love to come up with some examples are that actually useful to regular programmers.   Can you think about it and then just pick one example write it up in F#, showing the programmer how to solve a useful problem?



