# The Gödelian Toolkit: Systematically Testing Simple Languages

The Gödelian Toolkit is a small library that can help you generate all possible programs for a language you define.  Each program is assigned a unique number, and each number corresponds to a unique program.  If you pick a random number, you'll always get a valid program back.

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
