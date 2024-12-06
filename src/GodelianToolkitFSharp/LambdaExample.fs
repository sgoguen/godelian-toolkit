module LambdaExample

open GodelianTooklit

// Define a De Bruijn encoding of lambda terms
type Term =
    | Var of int // Variable reference
    | Lamda of Term // Abstraction - (Lambas)
    | App of Term * Term // Function application

let createSimple: bigint -> Term =
    combineChoices
        [ fun enc n -> Var(int n)
          fun enc n -> Lamda(enc n)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]

open Xunit
open TestUtils

[<Fact>]
let basicExamples () =
    checkExamples (
        [ 0I ==> Var(0)
          1I ==> Lamda(Var(0))
          2I ==> App(Var(0), Var(0))
          3I ==> App(Lamda(Var(0)), Var(0))
          4I ==> App(Var(0), Lamda(Var(0))) ]
    )


//  Let's assume we only wanted closed terms.  These are terms where
//  all of the variables are bound to a lambda function.

let isClosed (t: Term) =
    let rec isClosed' (t: Term) bound =
        match t with
        | Var i -> i < bound
        | Lamda t -> isClosed' t (bound + 1)
        | App(l, r) -> isClosed' l bound && isClosed' r bound

    isClosed' t 0

//  Let's define a lazy sequence of all closed terms
let allClosedTerms =
    Seq.initInfinite (fun i -> createSimple (bigint (i))) |> Seq.filter isClosed

let closedExamples =
    [ Var(0) ==> false
      Lamda(Var(0)) ==> true
      Lamda(Var(1)) ==> false
      Lamda(Lamda(Var 0)) ==> true
      Lamda(Lamda(Var 1)) ==> true
      Lamda(App(Var(0), Var(0))) ==> true ]

[<Fact>]
let checkClosedTerms () = checkMapping isClosed closedExamples

// Can we make a Godelian constructor that only generates closed terms?

// Yes - We can construct it by hand.
//
// The idea here is we know how many variables are available to us
// at each level of the term.
//
// If we haven't defined a lambda function yet, there are no variables
// available to us.  This means we're free to construct a lambda or an
// application.
//
// For every lambda term we construct, we allow the body of the lambda
// to have access to the variable is just defined and all of the variables
// that were available to us before.
//
// The big idea here is when constructing a term, we have options that
// are finite and options that are infinite.  Here, we pick our finite
// options first and map every number after that to our infinite options.
let makeClosedTerm n =
    let rec termFromIntRec (l: int) (n: bigint) =
        if n < 0I then
            failwith "Negative integer cannot be converted to a term"
        //  We pick our finite options first
        elif n <= (bigint l) then
            Var(int n)
        else
            //  We need to shift n down the number of finite options
            let n = n - bigint (l + 1)
            let opt = int (n % 2I)
            let d = n / 2I

            if opt = 0 then
                Lamda(termFromIntRec (l + 1) d)
            else
                let (x, y) = encodePair d
                App(termFromIntRec l x, termFromIntRec l y)

    termFromIntRec -1 n

[<Fact>]
let checkClosedTermMapping () =
    checkMapping
        makeClosedTerm
        [ 0I ==> Lamda(Var(0))
          1I ==> App(Lamda(Var(0)), Lamda(Var(0)))
          2I ==> Lamda(Lamda(Var(0)))
          3I ==> App(Lamda(Var(0)), App(Lamda(Var(0)), Lamda(Var(0)))) ]


//  Now if we really want to check our logic, let's create the inverse function
//  that converts a term back to a number.

let rec termToGodelNumber t =
    let rec closedTermToIntRec (d: int) (t: Term) : bigint =
        match t with
        | Var i -> (if i > d then failwithf "Invalid term" else bigint (i))
        | Lamda b -> (closedTermToIntRec (d + 1) b) * 2I + bigint (d) + 1I
        | App(x, y) ->
            let x = (closedTermToIntRec (d) x)
            let y = (closedTermToIntRec (d) y)
            let n = decodePair (x, y)
            n * 2I + bigint (d) + 2I

    let r =
        match t with
        | Var i -> failwithf "Invalid term"
        | Lamda b -> (closedTermToIntRec 0 b) * 2I
        | App(x, y) ->
            let x = closedTermToIntRec -1 x
            let y = closedTermToIntRec -1 y
            let n = decodePair (x, y)
            n * 2I + 1I

    r


//  Let's test our bijection with a test of 10000 terms
[<Fact>]
let testBijection () =
    for i in 0I .. 10000I do
        let t = makeClosedTerm i
        let i' = termToGodelNumber t
        Assert.Equal(i, i')


//  And we're good!  But here's the thing:  We want a nicer way of doing this.

//  Let's reconsider this tool that helps us create the inductive mapping.
//  What if that functionList wasn't static?
let combineChoices functionList =
    let length = bigint (List.length functionList)

    let rec chooseFunction n =
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction

//  What if we could decide what our options were at each level?
let combineChoicesWith getOptions =

    let rec chooseFunction n =
        let functionList = getOptions ()
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction

//  This is nice, but it doesn't seem to help us figure out which
//  variables are available to us at each level.
//
//  What if we had some contextual information?
let createSimple2: bigint -> Term =
    combineChoicesWith
    <| fun () ->
        [ fun enc n -> Var(int n)
          fun enc n -> Lamda(enc n)
          fun enc (Pair(l, r)) -> App(enc l, enc r) ]

//  Let's make a version that allows us to pass in initial contextual information
let combineChoicesWithContext initialContext getOptions =

    //  We add a parameter that now includes context
    let rec chooseFunction context n =
        let functionList = getOptions (context)
        let length = bigint (List.length functionList)
        let (d, r) = bigint.DivRem(n, length)
        let f = functionList.[int (r)]
        f chooseFunction d

    chooseFunction initialContext


//  Let's make a version that only generates closed terms
//  using our prototype
let createSimple3: bigint -> Term =
    let variablesAvailable = -1

    combineChoicesWithContext variablesAvailable (fun (vars) ->
        let makeVar enc n = Var(int n)
        let makeLamda enc n = Lamda(enc (vars + 1) n)
        let makeApp enc (Pair(l, r)) = App(enc vars l, enc vars r)

        if vars >= 0 then
            [ (fun enc n ->

                  if n <= (bigint vars) then
                      makeVar enc n
                  else
                      let n = n - bigint (vars + 1)
                      let (d, r) = bigint.DivRem(n, 2I)
                      if r = 0I then makeLamda enc d else makeApp enc d) ]
        else
            [ makeLamda; makeApp ])


//  Does createSimple3 work as our reference implementation (termToGodelNumber)?

[<Fact>]
let testSameAsReference () =
    for i in 0I .. 10000I do
        let t = makeClosedTerm i
        let t' = createSimple3 i
        Assert.Equal((i, t), (i, t'))

//  Ok, now I want a utility that will help me handle finite options
//  and infinite options.

let tryFiniteFirst (numberOfFiniteOptions: int) finiteConstuctor infiniteConstructors =
    let numberOfFiniteOptions = numberOfFiniteOptions - 1
    let length = bigint (List.length infiniteConstructors)

    if numberOfFiniteOptions >= 0 then
        [ (fun enc n ->
              if n <= (bigint numberOfFiniteOptions) then
                  finiteConstuctor enc n
              else
                  let n = n - (bigint (numberOfFiniteOptions + 1))
                  let (d, r) = bigint.DivRem(n, length)
                  let f = infiniteConstructors.[int r]
                  f enc d) ]
    else
        infiniteConstructors

//  Let's try using our tryFiniteFirst utility.
//  As you can see, our constructor is now 10 lines of formatted code.
let simplerConstructorYet: bigint -> Term =
    let variablesAvailable = 0

    combineChoicesWithContext variablesAvailable (fun (varsAvailable) ->
        let makeVar enc n = Var(int n)
        let makeLamda enc n = Lamda(enc (varsAvailable + 1) n)

        let makeApp enc (Pair(l, r)) =
            App(enc varsAvailable l, enc varsAvailable r)

        tryFiniteFirst varsAvailable makeVar [ makeLamda; makeApp ])

//  And simplerConstructorYet works as our reference implementation!
[<Fact>]
let testSameAsReference2 () =
    for i in 0I .. 10000I do
        let t = makeClosedTerm i
        let t' = simplerConstructorYet i
        Assert.Equal((i, t), (i, t'))

//  Should we try combining tryFiniteFirst and combineChoicesWithContext
//  into a single utility?

    