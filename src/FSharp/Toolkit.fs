module GodelianTooklit

open GodalToolkitFSharp.Math.Pairing

let encodePair = RosenbergStrong.encodePair
let decodePair = RosenbergStrong.decodePair
let (|Pair|) n = RosenbergStrong.encodePair n

let combineChoices functionList =
    let length = bigint (List.length functionList)

    let rec chooseFunction n =
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction

let pickOptions getOptions =

    let rec chooseFunction n =
        let functionList = getOptions ()
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction

