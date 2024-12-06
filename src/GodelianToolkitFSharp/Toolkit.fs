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

let pickOptions getOptions =

    let rec chooseFunction n =
        let functionList = getOptions()
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction    

open Xunit

module PairingTests = 

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

        for i in square .. square + 1000I..17I do
            let s = sqrt i
            Assert.Equal(s, root)

    [<Fact>]
    let ``encodePair and decodePair are bijective`` () =
        for i in 0I .. 1000I do
            let p = encodePair i
            let i' = p |> decodePair
            Assert.Equal(i, i')

