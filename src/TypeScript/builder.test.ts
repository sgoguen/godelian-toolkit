import { builder } from "./builder.ts";
import { encodePair } from "./toolkit.ts";
import { assertEquals } from "jsr:@std/assert";

type Term = 
    | { tag: "Var"; name: string }
    | { tag: "Lamda"; name: string; body: Term }
    | { tag: "App"; l: Term; r: Term };

function toString(t: Term): string {
    switch (t.tag) {
        case "Var":
            return t.name;
        case "Lamda":
            return `λ${t.name}.${toString(t.body)}`;
        case "App":
            return `(${toString(t.l)} ${toString(t.r)})`;
    }
}

class Variables {
    constructor(public varsAvailable: bigint) {}

    get Count(): number {
        return Number(this.varsAvailable);
    }

    Pick(n: number): string {
        return String.fromCharCode(97 + n);
    }

    NewVar(): [string, Variables] {
        const name = this.Pick(Number(this.varsAvailable));
        return [name, new Variables(this.varsAvailable + 1n)];
    }
}

const noVariables = new Variables(0n);

const makeTerm = builder.withContext<Variables, Term>(
    noVariables,
    (vars, def) => {
        return def
            .addBounded(vars.Count, (n) => ({ tag: "Var", name: vars.Pick(n) }))
            .addUnbounded((enc, n) => {
                const [name, newVars] = vars.NewVar();
                return { tag: "Lamda", name, body: enc(newVars, n) };
            })
            .addUnbounded((enc, n) => {
                const [a, b] = encodePair(n);
                const l = enc(vars, a);
                const r = enc(vars, b);
                return { tag: "App", l, r };
            });
    },
);

Deno.test("builder", () => {
    for (let i = 0n; i <= 100n; i++) {
        const e = makeTerm(i);
        console.log(`Godel Number: ${i} = ${toString(e)}`);
    }

    assertEquals(1, 1);
});
