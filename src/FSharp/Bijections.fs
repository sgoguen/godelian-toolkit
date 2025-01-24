module Bijections

open Xunit

open System
open System.Numerics
open GodalToolkitFSharp.Math

type BoundedBijection<'T> = {
    Cardinality: bigint
    Encode: 'T -> bigint
    Decode: bigint -> 'T
}

type UnboundedBijection<'T> = {
    Encode: 'T -> bigint
    Decode: bigint -> 'T
}

let bounded cardinality decode encode =
    { Cardinality = cardinality; Encode = encode; Decode = decode }

let fromList source = 
    let count = bigint(source |> List.length)
    let encode x = 
        let index = source |> List.findIndex (fun y -> y = x)
        bigint index
    let decode n = source.[int n]
    bounded count decode encode

let unbounded decode encode =
    { Encode = encode; Decode = decode }

let nats = unbounded id id

let productBoundedBounded b1 b2 =
    let cardinality = b1.Cardinality * b2.Cardinality
    let encode (a, b) = b1.Encode a * b2.Cardinality + b2.Encode b
    let decode n =
        let q, r = bigint.DivRem(n, b2.Cardinality)
        (b1.Decode q, b2.Decode r)
    bounded cardinality decode encode

let productBoundedUnbounded b1 b2 =
    let card = b1.Cardinality
    let encode (a, b) = 
        let x = b1.Encode a
        let y = b2.Encode b
        y * card + x
    let decode n =
        //  Let's cycle through the values of x
        let q, r = bigint.DivRem(n, card)
        (b1.Decode r, b2.Decode q)
    unbounded decode encode

let productUnboundedUnbounded b1 b2 =
    let encode (a, b) = 
        let x = b1.Encode a
        let y = b2.Encode b
        Pairing.RosenbergStrong.decodePair (x, y)
    let decode n =
        let x, y = Pairing.RosenbergStrong.encodePair n
        (b1.Decode x, b2.Decode y)
    unbounded decode encode

type Choice<'Tag, 'T> = Choice of 'Tag * 'T

let choiceBounded boundedBijections =
    //  Let's do a rolling sum of the cardinalities and store them in a list so we can find the offset
    //  with a binary search
    let boundedList : list<bigint * BoundedBijection<'a>> = 
        let mutable sum = 0I
        boundedBijections |> List.map (fun b -> sum <- sum + b.Cardinality; (sum, b))

    let length = boundedList |> List.length
    let halfway = length / 2

    //  Binary search to find correct bijection for the offset
    let binarySearch (n: bigint): int =
        assert (n >= 0I)
        let rec loop low high =
            if low > high then
                low
            else
                let mid = (low + high) / 2
                let (sum, _) = boundedList[mid]
                if sum < n then
                    loop (mid + 1) high
                elif sum > n then
                    loop low (mid - 1)
                else
                    assert (n >= sum)
                    mid
        let offset = loop 0 (length - 1)
        let (sum, _) = boundedList[offset]
        assert (n >= sum)
        offset

    let cardinality = boundedBijections |> List.sumBy (fun b -> b.Cardinality)
    let encode (Choice(tag, value)) =
        let (sum, bijection) = boundedList[tag]
        sum + bijection.Encode value

    let decode n =
        let offset = binarySearch n
        let (sum, bijection) = boundedList[offset]
        let tag = offset
        // assert (n >= sum)
        Choice(tag, bijection.Decode (n - sum))

    bounded cardinality decode encode

let choiceUnbounded unboundedBijections =
    let count = bigint(unboundedBijections |> List.length)
    let encode (Choice(tag, value)) =
        (unboundedBijections[tag].Encode value) * count + (bigint tag)

    let decode n =
        let q, r = bigint.DivRem(n, count)
        let tag = int r
        Choice(tag, unboundedBijections.[tag].Decode q)

    unbounded decode encode



module Tests =

    open System
    open Xunit

    let isBoundedBijection maxItems (bijection: BoundedBijection<'a>) =
        let c = bijection.Cardinality
        let maxItems = min (c - 1I) maxItems
        for n in 0I .. maxItems do
            let x = bijection.Decode n
            let n2 = bijection.Encode x
            Assert.Equal(n, n2)
        true

    let isUnboundedBijection maxItems (bijection: UnboundedBijection<'a>) =
        for n in 0I .. maxItems do
            let x = bijection.Decode n
            let n2 = bijection.Encode x
            Assert.Equal(n, n2)
        true
        



    [<Fact>]
    let ``Check fromList`` () =
        let abc = fromList [ "a"; "b"; "c" ]
        Assert.Equal(3I, abc.Cardinality)
        Assert.True(isBoundedBijection 100I abc)

    [<Fact>]
    let ``Unbounded nats can combine`` () =
        
        let nats = unbounded id id
        let product = productUnboundedUnbounded nats nats
        Assert.True(isUnboundedBijection 100I product)
        Assert.Equal(0I, product.Encode (0I, 0I))
        Assert.Equal(1I, product.Encode (0I, 1I))
        Assert.Equal(2I, product.Encode (1I, 1I))
        Assert.Equal(3I, product.Encode (1I, 0I))

    [<Fact>]
    let ``Bounded bijections can combine`` () =
        let abc = fromList [ "a"; "b"; "c" ]
        let def = fromList [ "d"; "e"; "f" ]
        let product = productBoundedBounded abc def
        Assert.True(isBoundedBijection 100I product)
        Assert.Equal(0I, product.Encode ("a", "d"))
        Assert.Equal(1I, product.Encode ("a", "e"))
        Assert.Equal(2I, product.Encode ("a", "f"))
        Assert.Equal(3I, product.Encode ("b", "d"))
        Assert.Equal(4I, product.Encode ("b", "e"))
        Assert.Equal(5I, product.Encode ("b", "f"))
        Assert.Equal(6I, product.Encode ("c", "d"))
        Assert.Equal(7I, product.Encode ("c", "e"))
        Assert.Equal(8I, product.Encode ("c", "f"))

    [<Fact>]
    let ``Bounded and unbounded bijections can combine`` () =
        let abc = fromList [ "a"; "b"; "c" ]
        let nats = unbounded id id
        let product = productBoundedUnbounded abc nats
        Assert.True(isUnboundedBijection 100I product)
        Assert.Equal(0I, product.Encode ("a", 0I))
        Assert.Equal(1I, product.Encode ("b", 0I))
        Assert.Equal(2I, product.Encode ("c", 0I))
        Assert.Equal(3I, product.Encode ("a", 1I))
        Assert.Equal(product.Decode(0I), ("a", 0I))
        Assert.Equal(product.Decode(1I), ("b", 0I))
        Assert.Equal(product.Decode(2I), ("c", 0I))
        Assert.Equal(product.Decode(3I), ("a", 1I))

    // [<Fact>]
    // let ``Choice bounded bijections`` () =
    //     let abc = fromList [ "a"; "b"; "c" ]
    //     let def = fromList [ "d"; "e"; "f" ]
    //     let choice = choiceBounded [ abc; def ]
    //     // Assert.True(isBoundedBijection 100I choice)
    //     Assert.Equal(choice.Decode 0I, Choice(0, "a"))
        // Assert.Equal(choice.Decode 1I, Choice(1, "d"))
        // Assert.Equal(choice.Decode 1I, Choice(1, "d"))
        // Assert.Equal(0I, choice.Encode (Choice(0, "a")))
        // Assert.Equal(1I, choice.Encode (Choice(1, "d")))
    //     Assert.Equal(2I, choice.Encode (Choice(0, "c")))
    //     Assert.Equal(3I, choice.Encode (Choice(1, "d")))
    //     Assert.Equal(4I, choice.Encode (Choice(1, "e")))
    //     Assert.Equal(5I, choice.Encode (Choice(1, "f")))

    // [<Fact>]
    // let ``Choice unbounded bijections`` () =
    //     let nats = unbounded id id
    //     let choice = choiceUnbounded [ nats; nats ]
    //     Assert.True(isUnboundedBijection 100I choice)
    //     Assert.Equal(0I, choice.Encode (Choice(0, 0I)))
    //     Assert.Equal(1I, choice.Encode (Choice(0, 1I)))
    //     Assert.Equal(2I, choice.Encode (Choice(1, 0I)))
    //     Assert.Equal(3I, choice.Encode (Choice(1, 1I)))
        

