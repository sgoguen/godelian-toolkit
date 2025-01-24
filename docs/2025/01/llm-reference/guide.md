# An LLM's Guide to the Godelian Toolkit

## Overview

The Gödelian Toolkit is a set of F# functions and types that facilitate constructing bijective (one-to-one) mappings between bigint values and instances of inductive data types (e.g., ASTs for programming languages).

This toolkit allows you to create what we want to call Bijective Gödelian constructors, which is a function that takes a bigint and returns an instance of some data type.  A Bijective Gödelian constructors should always assign a unique bigint to each unique instance (value-wise comparison, not reference-wise comparison)
so that it effectively indexes the set of all possible **VALID** instances of the data type that you want to construct.

## The Source Code

```fsharp
////////////////////////////////////////////////////////////////////////
///  The Gödelian Toolkit
////////////////////////////////////////////////////////////////////////

module GodelianTooklit

// The Rosenberg-Strong pairing function (balanced bijective mapping)
let decodePair (p: bigint * bigint) : bigint =
    let (x, y) = p
    let m = max x y
    m * m + m + x - y

// Inverse of the Rosenberg-Strong function (balanced bijective mapping)
let encodePair (z: bigint) : bigint * bigint =
    let m = sqrt (z)
    let m2 = m * m
    if z - m2 < m then (z - m2, m) else (m, m2 + 2I * m - z)

// Inverse of the Rosenberg-Strong function (in Active Pattern form) to help with brevity
let (|Pair|) = encodePair


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


type BoundedDomain<'T>(getValue: bigint -> 'T, encode: 'T -> bigint, size: bigint) = 
    member this.Cardinality = size
    member this.GetValue(n: bigint) = getValue n
    member this.Encode(value: 'T) = encode value
    // Define * operator to combine two bounded domains
    static member (*) (a: BoundedDomain<'T>, b: BoundedDomain<'U>) = 
        let size = a.Cardinality * b.Cardinality
        let getValue n = 
            let (x, y) = bigint.DivRem(a.Cardinality, n)
            (a.GetValue x, b.GetValue y)
        let encode (value: 'T * 'U) = 
            let (x, y) = value
            a.Encode x * b.Cardinality + b.Encode y
        BoundedDomain<'T * 'U>(getValue, encode, size)

and UnboundedDomain<'T>(getValue: bigint -> 'T, encode: 'T -> bigint) = 
    member this.GetValue(n: bigint) = getValue n
    member this.Encode(value: 'T) = encode value
    // Define * operator to combine two bounded domains
    static member (*) (a: UnboundedDomain<'T>, b: UnboundedDomain<'U>) = 
        let getValue n = 
            let (x, y) = encodePair n
            (a.GetValue x, b.GetValue y)
        let encode (value: 'T * 'U) = 
            let (x, y) = value
            let a = a.Encode x
            let b = b.Encode y
            decodePair (a, b)
        UnboundedDomain<'T * 'U>(getValue, encode)



