# Hi LLM

These are a list of files I want you to know about:

BEGIN README.md
----------------------------------------------

# The Gödelian Toolkit: Systematically Testing Simple Languages

The Gödelian Toolkit is a small library to helps you create a Gödelian numbering system for your toy programming language.

## Hello F# Community!

* If you're here for the F# Advent Calendar, I have preserved the original article here: [F# Advent Calendar 2024: The Gödelian Toolkit](https://sgoguen.github.io/godelian-toolkit/2024/12/fsharp-advent)

## Table of Contents

* [Quick Starts](#quick-starts)
    * [Hello Arithmetic!](#hello-Arithmetic)
    * [The Benefits of Gödelian Constructors](#the-benefits-of-gödelian-constructors)
    * [Hello Lambda Calculus!](#hello-lambda-calculus)
* [How does it work?](#how-does-it-work)
* [What's Next?](#whats-next)
* [About](#about)


## Quick Start

To get started, you can start by copying code from [godelian-toolkit.fsx](godelian-toolkit.fsx) into your F# Interactive session so you can start playing right away.

* [Learn the Basics with Hello Arithmetic!](./docs/quick-guides/hello-arithmetic.md)

## Hello Arithmetic!

Let's assume you had a simple math expression language:

```fsharp

//  Load the toolkit
#load "godelian-toolkit.fsx"
open GodelianTooklit

//  Define your expression type
type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr
```

We're now going to help you create what we call a **Gödelian constructor**.  A Gödelian constructor is a function that takes an arbitrarily large integer (bigint) and returns a unique instance of your expression type.

```fsharp
//  Here's our Gödelian constructor for Expr
let chooseExpr: bigint -> Expr =
    combineChoices
        [   fun enc n -> Num(n)
            fun enc n -> Neg(enc n)
            fun enc (Pair(l, r)) -> Add(enc l, enc r)
            fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]

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

## Hello Lambda Calculus!

```fsharp
// Define a De Bruijn encoding of lambda terms
type Term =
    | Var of string // Variable reference
    | Lamda of string * Term // Abstraction - (Lambas)
    | App of Term * Term // Function application


//  We can create a simplistic Gödelian constructor for this language

//  First, we need a function to convert integers to strings
let getName n = Strings.Alpha.fromInt n

// Now we can create our Gödelian constructor
let naiveConstructor: bigint -> Term =
    combineChoices
        [ fun enc varName -> Var(getName varName)
          fun enc (Pair(name, body)) -> Lamda(getName name ,enc body)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]

//  Let's take a look at some examples
for i in 0I .. 10I do
    let e = naiveConstructor i
    printfn "Godel Number: %A = %A" i e
```
While this might produce valid instances, there are some issues:

| Gödel Number   | Term            |
|----------------|----------------|
| 0              | Var "a"                                              |
| 1              | Lamda ("a", Var "a")                                 |
| 2              | App (Var "a", Var "a")                               |
| 3              | Var "b"                                              |
| 4              | Lamda ("a", Lamda ("a", Var "a"))                    |
| 5              | App (Var "a", Lamda ("a", Var "a"))                  |
| 6              | Var "c"                                              |
| 7              | Lamda ("b", Lamda ("a", Var "a"))                    |
| 8              | App (Lamda ("a", Var "a"), Lamda ("a", Var "a"))     |
| 9              | Var "d"                                              |
| 10             | Lamda ("b", Var "a")                                 |

* Every third term is a new variable reference to meaningless variable names.
* Term 7 is not a closed term because the body references a variable that is not bound.

We could fix this with something like De Bruijn indices, but I don't want the Gödelian Toolkit to force a particular representation on you.  I want to *TRY* to provide you with the tools to create valid and meaningful instances in a generalized algebraic way.

Let's imagine we had an immutable object that could keep track of all the variables we've used so far.  I'm just going to use a class type here and call it `Variables`.

```fsharp
type Variables(varsAvailable: bigint) = 
    member this.Count = int(varsAvailable)
    member this.Pick(i) = getName(i)
    member this.NewVar() = 
        let name = this.Pick(varsAvailable)
        (name, Variables(varsAvailable + 1I))
```

It's not complicated, it stores a list of strings, and has a method for 

```fsharp
let createClosedTerm: bigint -> Term =
    let initialVariables = Variables(0I)

    combineChoicesWithContext initialVariables (fun (vars) ->
        tryFiniteFirst
            vars.Count
            (fun i -> Var(vars.Pick(i)))
            [ fun enc n -> 
                let (name, newVars) = vars.NewVar()
                Lamda(name, enc newVars n)
              fun enc (Pair(l, r)) -> 
                App(enc vars l, enc vars r) ])
```




## The Benefits of Gödelian Constructors

1. **Completeness**: A Bijective Gödel Encoding ensures that every program is represented and uniquely encoded into a single integer.  This means, when you're systematically testing up to a certain size, you can be sure you're not missing any instances.
2. **Parallelism**:  Because every integer corresponds to a unique instance of your type, it is trivial to divide problem sets across multiple machines.
3. **Resumable Searches**:  Because every integer corresponds to a unique instance of your type, you can save your last position and resume a search at a later time.
4. **Bitset Encodable**:  Because every integer corresponds to a *valid* instance of your type, it makes the space of all of your programs contiguous.  This means, if you want to memoize anything about have a battery of tests test a range of integers, you can easily encode the results of your tests into a bitset.



* , which makes this approach useful for
//  situations where you might want to divide problems
//  across multiple machines, or if you want the ability
//  save your last position in a resume a search at a later
//  time.
//


Here are the first 10 instances for the above language:

| Gödel Number   | Expression       | F# Code                        |
|----------------|---------------|--------------------------------|
| 0              | 0             | Num 0                          |
| 1              | -(0)          | Neg (Num 0)                    |
| 2              | (0 + 0)       | Add (Num 0, Num 0)             |
| 3              | (0 * 0)       | Mul (Num 0, Num 0)             |
| 4              | 1             | Num 1                          |
| 5              | -(-(0))       | Neg (Neg (Num 0))              |
| 6              | (0 + -(0))    | Add (Num 0, Neg (Num 0))       |
| 7              | (0 * -(0))    | Mul (Num 0, Neg (Num 0))       |
| 8              | 2             | Num 2                          |
| 9              | -((0 + 0))    | Neg (Add (Num 0, Num 0))       |
| 10             | (-(0) + -(0)) | Add (Neg (Num 0), Neg (Num 0)) |


## Very Large Numbers

* This particular constructor creates a *bijection*, which means if you feed it an arbitrarily large integer and it will give you a *UNIQUE* object instance.
* Small numbers typically return smaller instances.
* Larger numbers typically return larger instances.



## What is a Gödelian Numbering System?

Let's assume you had a simple expression language defined by the following type:

```fsharp
type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr
```

The Gödelian Toolkit will help you create a function, that we call a **Godelian Constructor** to maps every integer to every possible program in your toy language.

| Gödel Number   | Program        |
|----------------|----------------|
| 0              | 0              |
| 1              | -(0)           |
| 2              | (0 + 0)        |
| 3              | (0 * 0)        |
| 4              | 1              |
| 5              | -(-(0))        |
| 6              | (0 + -(0))     |
| 7              | (0 * -(0))     |
| 8              | 2              |
| 9              | -((0 + 0))     |
| 10             | (-(0) + -(0))  |




 can help you generate all possible programs for a language you define.  Each program is assigned a unique number, and each number corresponds to a unique program.  If you pick a random number, you'll always get a valid program back.

We call these `Gödelian Constructors`.  

## What is a Gödelian Constructor?

A Gödelian Constructor is simply a function that takes some kind of big integer and returns a *unique* instance for that integer.  It's a bijective function that maps every integer to a unique program in your language.

## How do I use The Gödelian Toolkit?

```fsharp

//  Copy the code in this repository to your local machine
#load "godelian-toolkit.fsx"

//  Open the Godelian Toolkit
open GodelianTooklit

// Define a recursive type
type Expr =
    | Num of n: bigint
    | Neg of Expr
    | Add of Expr * Expr
    | Mul of Expr * Expr

// Create your Gödelian constructor!
let chooseExpr: bigint -> Expr =
    combineChoices
        [ fun enc n -> Num(n)
          fun enc n -> Neg(enc n)
          fun enc (Pair(l, r)) -> Add(enc l, enc r)
          fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]

// You may want an evaluator
let rec eval =
    function
    | Num n -> n
    | Neg e -> -(eval e)
    | Add(l, r) -> eval l + eval r
    | Mul(l, r) -> eval l * eval r

// May want to make them look pretty
let rec toString =
    function
    | Num n -> n.ToString()
    | Neg e -> sprintf "-(%s)" (toString e)
    | Add(l, r) -> sprintf "(%s + %s)" (toString l) (toString r)
    | Mul(l, r) -> sprintf "(%s * %s)" (toString l) (toString r)

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

//  Let's only print the first 20
for (i, e) in expressionsThatEqual42 |> Seq.truncate 20 do
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
// 31362 -> (22 + 20)
// 34162 -> (19 + 23)
// 34290 -> (23 + 19)
// 37154 -> (18 + 24)
// 37346 -> (24 + 18)
// 40274 -> (17 + 25)
// 40530 -> (25 + 17)
// 43093 -> -(-(-(-(42))))
// ...
```

## What's the Goal of The Gödelian Toolkit?

The primary goal of The Gödelian Toolkit is to simply educate people by showing them how to create these types of tools that let them systematically test inductive data types.  

## How does The Gödelian Toolkit work?

The Gödelian Toolkit gives you tools to help your create your own bijective functions with simple building blocks that help you build Gödelian constructors for:

    1. Product Types
    2. Sum Types
    3. Inductive Types with Recursive References

Let's look at an example:

```fsharp
type Expr =                   //  We have sum type with 3 cases
    | Var of string           //  The only non-recursive case
    | Lamba of string * Expr  //  Contains a product type (string * Expr)
    | App of Expr * Expr      //  Contains a product type with TWO recursive 
                              //  references!!!
```


## How do we handle Product Types?

In order to handle product types, we need a special pair of functions that can encode and decode an integer into a pair of integers.  These functions are generally called ***pairing functions***, but the ones you're looking at below are the ***Rosenberg-Strong*** pairing functions.

What's super important about all pairing functions is that they are **bijective**.  That means that if you give it a number, it will always give you back a pair of numbers.  And if you give it a pair of numbers, it will always give you back the original number.  The Rosenberg-Strong pairing function adds another quality.  It balances the left and right sides of the tuple so they grow in a balanced way.

```fsharp
//  Turns any positive bigint into a (bigint, bigint)
let encodePair (z: bigint): bigint * bigint = 
    let m = sqrt(z)
    let m2 = m * m
    if z - m2 < m then
        (z - m2, m)
    else
        (m, m2  + 2I * m - z)
```

Here's the decoder to the encoder above.  If you give it a pair of integers, it will give you back the original integer.

```fsharp
//  Turns any positive (bigint, bigint) into a unique bigint
let decodePair (p: bigint * bigint): bigint =
    let (x, y) = p
    let m = max x y
    m * m + m + x - y         
```

Let's try them out!

```fsharp
for i in 0I..19I do
    let p = encodePair i
    let d = decodePair p
    printfn "%A -> %A -> %A" i p d
```

Take a look at the following output.  Do you see a pattern?  Do you notice how we don't start introducing the number 2 in our pairs until we've exhausted all the combinations of 0 and 1?

```
0 -> (0, 0) -> 0
1 -> (0, 1) -> 1
2 -> (1, 1) -> 2
3 -> (1, 0) -> 3
4 -> (0, 2) -> 4
5 -> (1, 2) -> 5
6 -> (2, 2) -> 6
7 -> (2, 1) -> 7
8 -> (2, 0) -> 8
9 -> (0, 3) -> 9
```

Let's project the pairs onto a grid.  Start counting from 0 inside the grid and imagine a path between each successive number.  Do you notice how it constructs a square?

| (x, y)  | y=0 | y=1 | y=2 | y=3 | y=4 |
|---|---|---|---|---|---|
| x=0 | 0 | 1 | 4 | 9 | 16 |
| x=1 | 3 | 2 | 5 | 10 | 17 |
| x=2 | 8 | 7 | 6 | 11 | 18 |
| x=3 | 15 | 14 | 13 | 12 | 19 |
| x=4 | 24 | 23 | 22 | 21 | 20 |


I chose this particular pairing function because it's balanced, meaning the x and y values tend to grow at the same rate.

We want to construct a tuple, we first start by encoding a large integer into a pair of integers.  By using a balanced pairing function, we can ensure that the left and right values grow at the same rate.

## How does The Gödelian Toolkit handle Sum Types and Inductive Types?

Let's assume we want to encode the following type:

```fsharp
type Expr = 
    | Num of bigint
    | Neg of Expr
    | Add of Expr * Expr

//  We define our Gödelian constructor with a list of functions that accept
//  two parameters:
//     * enc - A function that will encode a bigint into an Expr
//     * n - The bigint we want to encode
let toExpression = combineChoices [ 
    fun enc n -> Num (n)     // Here we pass the bigint directly to Num
    fun enc n -> Neg (enc n) // We call 'enc' to n into an Expr first
    fun enc n ->             
        let (l, r) = encodePair n  // Here we get encode a pair of numbers
        Add (enc l, enc r)         // first into a pair of Exprs using 'enc'
]
```

When we run the following:

```fsharp
for i in 0I..10I do
    let e = bigintToExpr i
    printfn "%A -> %A" i e        
```

We'll get the following output:

```
0 -> Num 0
1 -> Neg (Num 0)
2 -> Add (Num 0, Num 0)
3 -> Num 1
4 -> Neg (Neg (Num 0))
5 -> Add (Num 0, Neg (Num 0))
6 -> Num 2
7 -> Neg (Add (Num 0, Num 0))
8 -> Add (Neg (Num 0), Neg (Num 0))
9 -> Num 3
10 -> Neg (Num 1)
```

## How does `combineChoices` work?

Our `combineChoices` function takes a list of **cooperatively-recursive** functions and give us back a Gödelian constructor that creates a unique Expr for every bigint.

```fsharp
// toExpression: bigint -> Expr
let toExpression = combineChoices [ 
    fun enc n -> Num (n)
    fun enc n -> Neg (enc n)
    fun enc n -> 
        let (l, r) = encodePair n
        Add (enc l, enc r) 
]
```

What do I mean by cooperatively-recursive?

In this scenario, when the second or third function calls `enc`, it's calling a function that chooses which function to call.  It may call itself, or it may call another function.  It depends on that value of the number passed to it.

## Can you show me the code for `combineChoices`?

Sure, combineChoices takes a list of functions.  Based on the value of `n`, it will pick the which function to call.  In this case, we use DivRem to divide `n` by the number of functions available to us.  We use the remainder to pick the function and
we feed the divisor to the function.

```fsharp
let combineChoices functionList  =
    //  Let's save the length of the function list
    let length = bigint(List.length functionList)

    //  This is the main recursive function that picks the right function
    //  convert out bigint into an object
    let rec chooseFunction n =
        //  We divide our input by n to get the divisor and the remainder
        let (d, r) = bigint.DivRem(n, length)
        //  We pick the function we want to use with the remainder
        let f = functionList.[int(r)]
        //  And we call the function with the divisor
        //  BUT we pass the chooseFunction to the function so the selected
        //  invoke this cooporative recursion system.
        f chooseFunction d

    chooseFunction
```

## What's Next?

* Check out [godelian-toolkit.fsx](godelian-toolkit.fsx)
* Or look [math-example.fsx](math-example.fsx)

## Feel Free to Reach Out! 

Please do not hesitate to reach out.  I would love to hear from you if you have questions, comments, ideas or if you want to talk shop.

Also, feel free to add issues, bugs, pull requests or to send me your use cases where this doesn't work for you.  If you have an interesting use case, I'm probably interested in figuring it out somehow.  

You can also contact me on BlueSky @sgoguen.bsky.social or Twitter @sgoguen.



------------------------

## Appendix


### Bijective Gödel Encoding

[Gödel encoding](https://en.wikipedia.org/wiki/G%C3%B6del_numbering) is a way to encode a program as a number.  It was invented by Kurt Gödel in the 1930s as a way to prove some really interesting things about logic and mathematics.

But there was always something I personally didn't like about the original Gödel encoding:  If you were to pick a random number, you were more likely to get gibberish nonsense rather than a valid program.

**Bijective** Gödel Encodings fix that.  No matter what number you pick, you'll always get a syntactically valid program every time!  That Bijective word is an important word, because it means that *EVERY* number corresponds to a **UNIQUE** program.  BUT, it also means that **ALL** programs written in that language correspond to a number.

BTW, If you're interested in Bijective Gödel Encodings, you should check 
out [Paul Tarau's work](https://ptarau.github.io/).  First, his Prolog 
implementations are super cool.  Second, how he creates Bijective Gödel Encodings in Prolog and explores these spaces is mind-blowingly interesting.

For his papers, check him out on [Google Scholar](https://scholar.google.com/scholar?q=Bijective+Godel+Encoding).




BEGIN godelian-toolkit.fsx
----------------------------------------------

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


let combineChoices functionList =
    let length = bigint (List.length functionList)

    let rec chooseFunction n =
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction


let combineChoicesWithContext getOptions initialContext =

    //  We add a parameter that now includes context
    let rec chooseFunction context n =
        let functionList = getOptions (context)
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction initialContext


let tryFiniteFirst (numberOfFiniteOptions: int) finiteConstuctor infiniteConstructors =
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

module Counter = 
    let makeCounter () =
        let mutable counter = 0I
        fun () ->
            let result = counter
            counter <- counter + 1I
            result


module Strings = 
    module Alpha = 
        let rec fromInt n =
            if n < 26I then
                string (char (int 'a' + int n))
            else
                fromInt (n / 26I) + string (char (int 'a' + int (n % 26I)))


[<AbstractClass>]
type FiniteOptions<'context,'selection>(context: 'context) = 
    abstract member Count : bigint
    abstract member Pick : bigint -> 'selection
    member this.Choose(n: bigint, otherOptions) = 
        
        let rec chooseFunction (n: bigint) =
            let length = this.Count
            let (d: bigint, r: bigint) = bigint.DivRem(n, length)
            if d = 0I then
                this.Pick r
            else
                let f = otherOptions |> List.item (int r)
                f context chooseFunction d

        chooseFunction n



// module Finite = 
    
//     type FiniteOptions(varsAvailable: bigint) = 
//         member this.Count = int(varsAvailable)
//         member this.Pick(i) = getName(i)
//         member this.NewVar() = 
//             let name = this.Pick(varsAvailable)
//             (name, Variables(varsAvailable + 1I))

//     type FiniteOptions<'a,'b>(context: 'a, getCount, pickOptions) = 
//         member this.Count : int = getCount context
//         member this.Pick(i: int): 'b = pickOptions i context

//     type ListOptions<'a>(list: 'a list) = 
//         inherit FiniteOptions<'a list, 'a>(list, List.length, List.item)

//     type AutoListOptions<'a>(list: 'a list) = 
//         inherit FiniteOptions<'a list, 'a>(list, List.length, List.item)


BEGIN lambda-example-2.fsx
----------------------------------------------

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
for i in 0I .. 10I do
    let e = naiveConstructor i
    printfn "Godel Number: %A = %A" i (e.ToString())

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


//  How many of these terms are valid closed terms?


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


for i in 0I .. 100I do
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


//  This is work-in-progress and I would love to hear your ideas.

//  How do you think this example could be improved?


