import { test } from "node:test";
import assert from "node:assert/strict";
import { combineChoices, encodePair } from "./toolkit";

// // Define a recursive type
// type Expr =
//     | Num of n: bigint
//     | Neg of Expr
//     | Add of Expr * Expr
//     | Mul of Expr * Expr

type Expr =
    { tag: "Num", n: number }
    | { tag: "Neg", e: Expr }
    | { tag: "Add", l: Expr, r: Expr }
    | { tag: "Mul", l: Expr, r: Expr }

// // Create your universal constructor!
// let chooseExpr: bigint -> Expr =
//     combineChoices
//         [   fun enc n -> Num(n)
//             fun enc n -> Neg(enc n)
//             fun enc (Pair(l, r)) -> Add(enc l, enc r)
//             fun enc (Pair(l, r)) -> Mul(enc l, enc r) ]

function chooseExpr(n: bigint): Expr {
    return combineChoices<Expr>([
        (enc, n) => ({ tag: "Num", n: (parseInt(n.toString())) }),
        (enc, n) => ({ tag: "Neg", e: enc(n) }),
        (enc, n) => {
            let [l, r] = encodePair(n);
            return { tag: "Add", l: enc(l), r: enc(r) };
        },
        (enc, n) => {
            let [l, r] = encodePair(n);
            return { tag: "Mul", l: enc(l), r: enc(r) };
        }
    ], n);
}

function testInjectivity<T>(f: (n: bigint) => T) {
    const uniqueValues = new Set<string>();
    for (let i = 0n; i < 100n; i++) {
        const value = f(i);
        const t = JSON.stringify(value);
        uniqueValues.add(t);
    }
    assert.strictEqual(uniqueValues.size, 100);
}

test("chooseExpr", () => {
    testInjectivity(chooseExpr);
});

