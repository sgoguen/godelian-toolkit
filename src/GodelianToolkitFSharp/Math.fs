module GodalToolkitFSharp.Math

module BigInteger =
    open System.Numerics

    let sqrtSignedRem x =
        let (x, negate) = if x < -1I then (-x, true) else (x, false)

        let rec loop previous =

            // current = (previous + x / previous) / 2
            let current = (previous + x / previous) >>> 1

            if abs (previous - current) < 2I then
                current
            else
                loop current

        // guess = 10 ^ ((log10(x + 1) + 1) / 2)
        let guess = 10I ** (((int (BigInteger.Log10(x + 1I))) + 1) >>> 1)
        let r = loop guess
        let r2 = r * r

        match compare r2 x with
        | 0 -> (r, 0I)
        | 1 ->
            let root = r - 1I in
            let rem = if negate then -(x - root * root) else (x - root * root)
            (root, rem)
        | _ -> (r, (if negate then -(x - r2) else (x - r2)))

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


    let z = sqrt 123098233423I

module Pairing = 

    module RosenbergStrong =
        open BigInteger

        let encodePair (z: bigint) : bigint * bigint =
            let m = sqrt (z)
            let m2 = m * m
            if z - m2 < m then (z - m2, m) else (m, m2 + 2I * m - z)

        let decodePair (p: bigint * bigint) : bigint =
            let (x, y) = p
            let m = max x y
            m * m + m + x - y

        let (|Pair|) = encodePair

        module Tests =
            
            open Xunit

            [<Fact>]
            let ``sqrt rounds down`` () =
                for i in 0I .. 1000I do
                    let s = sqrt i
                    let s1 = s + 1I
                    Assert.True(s * s <= i && i < s1 * s1)

            [<Fact>]
            let ``sqrt works for big numbers`` () =
                let root = 39872322340982340984I
                let square = root * root

                for i in square .. square + 1000I .. 17I do
                    let s = sqrt i
                    Assert.Equal(s, root)

            [<Fact>]
            let ``encodePair and decodePair are bijective`` () =
                for i in 0I .. 1000I do
                    let p = encodePair i
                    let i' = p |> decodePair
                    Assert.Equal(i, i')
