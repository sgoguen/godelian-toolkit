import { test } from "node:test";
import assert from "node:assert/strict";
import { combineChoices, encodePair } from "./toolkit";

// Define a simple De Bruijn encoding of lambda terms
type Term =
  | { tag: "Var"; i: number }
  | { tag: "Lamda"; b: Term }
  | { tag: "App"; l: Term; r: Term };

function createSimple(n: bigint): Term {
  return combineChoices<Term>(
    [
      (_enc, n) => ({ tag: "Var", i: Number(n) }),
      (enc, n) => ({ tag: "Lamda", b: enc(n) }),
      (enc, n) => {
        const [l, r] = encodePair(n);
        return { tag: "App", l: enc(l), r: enc(r) };
      },
    ],
    n,
  );
}

function toString(t: Term): string {
  switch (t.tag) {
    case "Var":
      return `Var(${t.i})`;
    case "Lamda":
      return `Lamda(${toString(t.b)})`;
    case "App":
      return `App(${toString(t.l)}, ${toString(t.r)})`;
    default:
      const _exhaustive: never = t;
      return _exhaustive;
  }
}

test("createSimple mappings", () => {
  const cases: [bigint, Term][] = [
    [0n, { tag: "Var", i: 0 }],
    [1n, { tag: "Lamda", b: { tag: "Var", i: 0 } }],
    [2n, { tag: "App", l: { tag: "Var", i: 0 }, r: { tag: "Var", i: 0 } }],
    [3n, { tag: "Var", i: 1 }],
    [4n, { tag: "Lamda", b: { tag: "Lamda", b: { tag: "Var", i: 0 } } }],
    [5n, { tag: "App", l: { tag: "Var", i: 0 }, r: { tag: "Lamda", b: { tag: "Var", i: 0 } } }],
  ];

  for (const [n, expected] of cases) {
    const actual = createSimple(n);
    assert.deepStrictEqual(
      actual,
      expected,
      `createSimple(${n}) = ${toString(actual)}`,
    );
  }
});

