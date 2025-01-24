//  I want to play around with the idea of constructing function domains with a fluent interface
//  to explore how one might transition between different domain types when appling different methods.

module AbstractSets = 
    


open System
open System.Collections.Generic

type SetDomain<'d when 'd : comparison>(getSet: ISet<'d>) = 
    member this.Contains(x: 'd) = getSet.Contains(x)
    new(getInstance: bigint -> 'd) = 
        SetDomain<'d>((fun () -> Set([ for i in 0I..bigint(Int32.MaxValue) do yield getInstance i ])))

/// We could define a domain as a sequence of elements, but this means they're not indexed.
/// While we can iterate over them, we never know if an element is IN the domain, until we've seen it.
type SeqDomain<'d>(getSeq: unit -> seq<'d>) = 
    member this.Seq = getSeq()
    interface seq<'d> with
        member this.GetEnumerator() = getSeq().GetEnumerator()
        member this.GetEnumerator() = (getSeq() :> System.Collections.IEnumerable).GetEnumerator()
    new(getInstance: bigint -> 'd) = 
        SeqDomain<'d>((fun () -> seq { for i in 0I..bigint(Int32.MaxValue) do yield getInstance i }))


and InfiniteDomain<'d>(getInstance: bigint -> 'd) = 
    inherit SeqDomain<'d>(getInstance)
    member this.Get(index: bigint) = getInstance index

and FiniteDomain<'d>(cardinality: bigint, create: bigint -> 'd) = 
    inherit SeqDomain<'d>(fun () -> seq { for i in 0I .. cardinality - 1I do yield create i })
    member this.TryGet(index: bigint) = create index
    member this.Cardinality = cardinality


//////////////////////////////////////////////////////////////////////////////////

type SeqDomain<'d> with
    member this.Take(cardinality: int) = 
        let array = this.Seq |> Seq.take (int cardinality) |> Seq.toArray
        FiniteDomain<'d>(cardinality, fun i -> array.[int i])

    member this.TakeUntil(predicate: 'd -> bool) = 
        let array = this.Seq |> Seq.takeWhile (fun x -> not (predicate x)) |> Seq.toArray
        FiniteDomain<'d>(array.Length, fun i -> array.[int i])

type InfiniteDomain<'d> with
    member this.Map<'d2>(f: 'd -> 'd2) = InfiniteDomain<'d2>(fun i -> f (this.Get i))
    member this.Filter(predicate: 'd -> bool) = SeqDomain<'d>(fun i -> this.Seq |> Seq.filter predicate)
    member this.LimitTo(count: bigint) = FiniteDomain<'d>(count, this.Get)


type Domain<'d> = 
    static member Seq(source: seq<'d>) = source
    static member Finite(cardinality: bigint, getInstance: bigint -> 'd) = FiniteDomain<'d>(cardinality, getInstance)
    static member Infinite(getInstance: bigint -> 'd) = InfiniteDomain<'d>(getInstance)


type Fn<'a, 'b>(f: 'a -> 'b) = 
    member this.Invoke(x) = f x
    member this.DefineDomain(enumDomain: seq<'a>) = 
        FnWithDomain<'a, 'b, seq<'a>>(f, enumDomain)



and FnWithDomain<'a, 'b, 'domain when 'domain :> seq<'a>>(f: 'a -> 'b, enumDomain: 'domain) = 
    inherit Fn<'a, 'b>(f)
    member this.Domain: 'domain = enumDomain


and FnWithBoundedDomain<'domain, 'codomain>(f: 'domain -> 'codomain, domain: FiniteDomain<'domain>) = 
    inherit Fn<'domain, 'codomain>(f)
    member this.Domain = domain