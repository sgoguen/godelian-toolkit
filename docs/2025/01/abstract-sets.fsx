

//  Let's explore the different ways we can define sets

//  1. We can use predicates:
type PredicateSet<'d>(predicate: 'd -> bool) = 
    member this.Contains(x: 'd) = predicate x

//  2. We can use a finite sequence of elements:
type FiniteSet<'d when 'd : comparison>(max: int, getSeq: unit -> seq<'d>) = 
    let internalSet = getSeq() |> Seq.take max |> Set.ofSeq
    member this.Seq = getSeq()
    interface seq<'d> with
        member this.GetEnumerator() = getSeq().GetEnumerator()
        member this.GetEnumerator() = (getSeq() :> System.Collections.IEnumerable).GetEnumerator()
