////////////////////////////////////////////////////////////////////////
///  The Gödelian Toolkit
////////////////////////////////////////////////////////////////////////

export function sqrt(z: bigint): bigint {
    if (z < 0n) {
        throw new Error("Cannot compute the square root of a negative number");
    } else if (z === 0n) {
        return 0n;
    } else {
        let x = z;
        while (true) {
            const nextX = (x + z / x) / 2n;
            if (nextX >= x) {
                return x;
            } else {
                x = nextX;
            }
        }
    }
}

export function encodePair(z: bigint): [bigint, bigint] {
    const m = sqrt(z);
    const m2 = m * m;
    if (z - m2 < m) {
        return [z - m2, m];
    } else {
        return [m, m2 + 2n * m - z];
    }
}

export function decodePair(p: [bigint, bigint]): bigint {
    const [x, y] = p;
    const m = x > y ? x : y;
    return m * m + m + x - y;
}

export function combineChoices<T>(
    functionList: ((chooseFunction: (n: bigint) => T, n: bigint) => T)[],
    n: bigint,
): T {
    const length = BigInt(functionList.length);

    function chooseFunction(n: bigint): T {
        const d = n / length;
        const r = n % length;
        const f = functionList[Number(r)];
        return f(chooseFunction, d);
    }

    return chooseFunction(n);
}

export function combineChoicesWithContext<T, U>(
    getOptions: (
        context: T,
    ) => ((chooseFunction: (context: T, n: bigint) => U, n: bigint) => U)[],
    initialContext: T,
): (n: bigint) => U {
    function chooseFunction(context: T, n: bigint): U {
        const functionList = getOptions(context);
        const length = BigInt(functionList.length);
        const d = n / length;
        const r = n % length;
        const f = functionList[Number(r)];
        return f(chooseFunction, d);
    }

    return (n: bigint) => chooseFunction(initialContext, n);
}

export function tryFiniteFirst<T, U>(
    numberOfFiniteOptions: number,
    finiteConstuctor: (n: number) => U,
    infiniteConstructors:
        ((ctx: T, n: bigint) => U)[],
): ((ctx: T, n: bigint) => U)[] {
    numberOfFiniteOptions = numberOfFiniteOptions - 1;
    const length = BigInt(infiniteConstructors.length);

    if (numberOfFiniteOptions >= 0) {
        return [(chooseFunction, n) => {
            if (n <= BigInt(numberOfFiniteOptions)) {
                return finiteConstuctor(Number(n));
            } else {
                n = n - BigInt(numberOfFiniteOptions + 1);
                const d = n / length;
                const r = n % length;
                const f = infiniteConstructors[Number(r)];
                return f(chooseFunction, d);
            }
        }];
    } else {
        return infiniteConstructors;
    }
}
