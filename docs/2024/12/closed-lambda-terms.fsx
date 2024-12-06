////////////////////////////////////////////////////////////////////////
///  The Gödelian Toolkit
////////////////////////////////////////////////////////////////////////

module GodelianTooklit = 

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


