import {
    combineChoices,
    combineChoicesWithContext,
    encodePair,
    tryFiniteFirst,
} from "./toolkit.ts";
import { assertEquals } from "jsr:@std/assert";

// // Define a simple Lambda Calculus language
// // with a simple default syntax

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

// // Let's use this utility from the toolkit to turn numbers into strings
// // for our variable names.
// let getName n = Strings.Alpha.fromInt n

function getName(n: number): string {
    return String.fromCharCode(97 + n);
}

// //  We can create a naive constructor that generates all terms
// //  Unfortunately, this will generate terms that are not closed.
// //  This means it will define functions that reference variables
// //  that are not bound by a lambda function.  :'(

function naiveConstructor(n: bigint): Term {
    return combineChoices<Term>([
        (enc, varName) => ({ tag: "Var", name: getName(Number(varName)) }),
        (enc, n) => {
            const [name, body] = encodePair(n);
            return ({
                tag: "Lamda",
                name: getName(Number(name)),
                body: enc(body),
            });
        },
        (enc, n) => {
            const [l, r] = encodePair(n);
            return ({ tag: "App", l: enc(l), r: enc(r) });
        },
    ], n);
}

Deno.test("naiveConstructor", () => {
    for (let i = 0n; i <= 10n; i++) {
        const e = naiveConstructor(i);
        console.log(`Godel Number: ${i} = ${toString(e)}`);
    }
});

// Produces examples like:
// Godel Number: 0 = "a"
// Godel Number: 1 = "λa.a"
// Godel Number: 2 = "(a a)"
// Godel Number: 3 = "b"
// Godel Number: 4 = "λa.λa.a"
// Godel Number: 5 = "(a λa.a)"
// Godel Number: 6 = "c"
// Godel Number: 7 = "λb.λa.a"
// Godel Number: 8 = "(λa.a λa.a)"
// Godel Number: 9 = "d"
// Godel Number: 10 = "λb.a"

//  Unfortunately, this will generate terms that are not closed.

//  First, let's define a function that checks if a term is closed.

function isClosed(t: Term): boolean {
    function isClosedRec(t: Term, variables: Set<string>): boolean {
        switch (t.tag) {
            case "Var":
                return variables.has(t.name);
            case "Lamda":
                return isClosedRec(t.body, new Set([...variables, t.name]));
            case "App":
                return isClosedRec(t.l, variables) &&
                    isClosedRec(t.r, variables);
        }
    }

    return isClosedRec(t, new Set());
}

Deno.test("isClosed", () => {
    for (let i = 0n; i <= 50n; i++) {
        const e = naiveConstructor(i);
        if (isClosed(e)) {
            console.log(`Godel Number: ${i} = ${toString(e)}`);
        }
    }
});

// Godel Number: 1 = "λa.a"
// Godel Number: 4 = "λa.λa.a"
// Godel Number: 7 = "λb.λa.a"
// Godel Number: 8 = "(λa.a λa.a)"
// Godel Number: 13 = "λa.(a a)"
// Godel Number: 22 = "λc.λa.a"
// Godel Number: 31 = "λb.b"
// Godel Number: 43 = "λd.λa.a"
// Godel Number: 49 = "λa.λa.λa.a"

//  This looks better, but it's worth pointing out that #1 and #31 are
//  the same term.  While the names are different, the terms are the same.
//  This is called alpha equivalence.

//  We can fix this by using some new tools in the toolkit.
//  1. While we're constructing an instance of our inductive type,
//     we add a notion of context that we can keep track of while
//     we're constructing the term.  In this case, we'll keep track
//     of the number of variables that are available to bind.
//  2. We also need to treat finite and infinite options differently.
//     Here we use tryFiniteFirst to map the first n options to some
//     finite constructor, everything else is mapped to an infinite
//     constructor.

class Variables {
    constructor(public varsAvailable: bigint) {}

    get Count(): number {
        return Number(this.varsAvailable);
    }

    Pick(i: number): string {
        return getName(i);
    }

    NewVar(): [string, Variables] {
        const name = this.Pick(Number(this.varsAvailable));
        return [name, new Variables(this.varsAvailable + 1n)];
    }
}

// let createClosedTerm: bigint -> Term =
//     let initialVariables = Variables(0I)

//     initialVariables
//     |> combineChoicesWithContext (fun (vars) ->
//         tryFiniteFirst
//             vars.Count
//             (fun i -> Var(vars.Pick(i)))
//             [ fun enc n ->
//                   let (name, newVars) = vars.NewVar()
//                   Lamda(name, enc newVars n)
//               fun enc (Pair(l, r)) -> App(enc vars l, enc vars r) ])

function createClosedTerm(n: bigint): Term {
    const initialVariables = new Variables(0n);

    return combineChoicesWithContext<Variables, Term>((vars) => {
        return tryFiniteFirst(
            vars.Count,
            makeVar,
            [
                (enc, n) => {
                    const [name, newVars] = vars.NewVar();
                    return ({ tag: "Lamda", name, body: enc(newVars, n) });
                },
                (enc, n) => ({ tag: "App", l: enc(vars, n), r: enc(vars, n) }),
            ],
        );

        function makeVar(n: number): Term {
            return { tag: "Var", name: vars.Pick(n) };
        }

    }, initialVariables)(n);
}

Deno.test("createClosedTerm", () => {
    for (let i = 0n; i <= 10n; i++) {
        const e = createClosedTerm(i);
        console.log(`Godel Number: ${i} = ${toString(e)}`);
    }
});

// // Produces examples like:

// // Godel Number: 0 = "λa.a"
// // Godel Number: 1 = "(λa.a λa.a)"
// // Godel Number: 2 = "λa.λb.a"
// // Godel Number: 3 = "(λa.a (λa.a λa.a))"
// // Godel Number: 4 = "λa.(a a)"
// // Godel Number: 5 = "((λa.a λa.a) (λa.a λa.a))"
// // Godel Number: 6 = "λa.λb.b"
// // Godel Number: 7 = "((λa.a λa.a) λa.a)"
// // Godel Number: 8 = "λa.(a λb.a)"
// // Godel Number: 9 = "(λa.a λa.λb.a)"
// // Godel Number: 10 = "λa.λb.λc.a"
// // Godel Number: 11 = "((λa.a λa.a) λa.λb.a)"

Deno.test("Final Test", () => {
});
