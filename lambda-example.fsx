#load "godelian-toolkit.fsx"

open GodelianTooklit

// Define a De Bruijn encoding of lambda terms
type Term =
    | Var of int // Variable reference
    | Lamda of Term // Abstraction - (Lambas)
    | App of Term * Term // Function application

//  We can create a naive constructor that generates all terms
//  Unfortunately, this will generate terms that are not closed.
//  This means it will define functions that reference variables
//  that are not bound by a lambda function.  :'(
let naiveConstructor: bigint -> Term =
    combineChoices
        [ fun enc n -> Var(int n)
          fun enc n -> Lamda(enc n)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]

//  We can fix this by using some new tools in the toolkit.
//  1. While we're constructing an instance of our inductive type,
//     we add a notion of context that we can keep track of while
//     we're constructing the term.  In this case, we'll keep track
//     of the number of variables that are available to bind.
//  2. We also need to treat finite and infinite options differently.
//     Here we use tryFiniteFirst to map the first n options to some
//     finite constructor, everything else is mapped to an infinite
//     constructor.
let createTerm: bigint -> Term =
    let variablesAvailable = 0

    combineChoicesWithContext variablesAvailable (fun (varsAvailable) ->
        tryFiniteFirst
            varsAvailable
            Var
            [ fun enc n -> Lamda(enc (varsAvailable + 1) n)
              fun enc (Pair(l, r)) -> App(enc varsAvailable l, enc varsAvailable r) ])


//  This is work-in-progress and I would love to hear your ideas.

//  How do you think this example could be improved?