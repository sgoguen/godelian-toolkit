module Builder

open System

type BoundedFunctionDef<'T> =
    { StartAt: int
      Cardinality: int
      F: int -> 'T }

type IChoices<'CTX, 'T> =
    abstract MaxBounded: int
    abstract BoundedConstructors: List<BoundedFunctionDef<'T>>
    abstract UnboundedConstructors: List<Func<Func<'CTX, bigint, 'T>, bigint, 'T>>


type IChoiceBuilder<'CTX, 'T> =
    abstract Bud: max: int * f: (Func<int, 'T>) -> IBuiltChoices<'CTX, 'T>
    abstract Branch: f: Func<Func<'CTX, bigint, 'T>, bigint, 'T> -> IBuiltChoices<'CTX, 'T>

and IBuiltChoices<'CTX, 'T> =
    inherit IChoiceBuilder<'CTX, 'T>
    inherit IChoices<'CTX, 'T>


type IConstructorBuilder<'T> =
    abstract WithContext<'CTX, 'T> : 
        'CTX * ( 'CTX -> IChoiceBuilder<'CTX,'T> -> IChoices<'CTX,'T> ) 
        -> Func<bigint,'T>


type ConstructorBuilderImpl<'T>() =
    interface IConstructorBuilder<'T> with
        member this.WithContext(ctx: 'CTX, getOptions) = 

            let initialContext = ctx

            let getItem(n: bigint) =

                let choices = ChoiceBuilderImpl(ctx, getOptions)

                // Build the choices each time we invoke                
                let options = getOptions ctx choices

                let maxBounded = bigint(options.MaxBounded)
                if n < maxBounded then
                    // Find the right bounded function
                    let n2 = int n
                    let mutable found = None
                    for o in options.BoundedConstructors do
                        let startAt = o.StartAt
                        let endAt = o.StartAt + o.Cardinality
                        if n2 >= startAt && n2 < endAt then
                            let offset = n2 - startAt
                            found <- Some (o.F(offset))
                            break
                    match found with
                    | Some result -> result
                    | None -> failwithf "No bounded function found for n: %A" n
                else
                    // Find the right unbounded function
                    let length = BigInteger(options.UnboundedConstructors.Count)
                    let d = n / length
                    let r = n % length
                    let f = options.UnboundedConstructors.[int r]
                    // "enc" is a local function that calls WithContext again
                    f.Invoke(
                        Func<'CTX, BigInteger, 'T>(
                            fun newContext bigN -> 
                                // Recursively call WithContext
                                (this :> IConstructorBuilder).WithContext(newContext, getOptions).Invoke(bigN)
                        ), 
                        d
                    )
            Func<_,_>(getItem)

and ChoiceBuilderImpl<'CTX, 'T>(ctx: 'CTX,initialize: 'CTX -> IChoiceBuilder<'CTX,'T> -> IChoices<'CTX,'T>) =
    
    let boundedConstructors = List<BoundedFunctionDef<'T>>()
    let unboundedConstructors = List<Func<Func<'CTX, BigInteger,'T>, BigInteger,'T>>()

    // We call "initialize(ctx, this)" so the user can register bounded/unbounded.
    do initialize(ctx, upcast this) |> ignore
    
    // We'll track the maximum bounded so far
    let mutable maxBounded = 0

    // We implement the IChoiceBuilder interface:
    interface IChoiceBuilder<'CTX,'T> with
        member _.Bud(max, f) =
            let def = BoundedFunctionDef<'T>()
            def.StartAt <- maxBounded
            def.Cardinality <- max
            def.F <- f
            boundedConstructors.Add(def)
            maxBounded <- maxBounded + max
            upcast this

        member _.Branch(f) =
            unboundedConstructors.Add(f)
            upcast this

    // We also implement IChoices:
    interface IChoices<'CTX,'T> with
        member _.MaxBounded = maxBounded
        member _.BoundedConstructors = boundedConstructors
        member _.UnboundedConstructors = unboundedConstructors

    // Finally, IBuiltChoices just merges those two interfaces:
    interface IBuiltChoices<'CTX,'T>
        inherit IChoiceBuilder<'CTX,'T>
        inherit IChoices<'CTX,'T>

// // ----------------------------------
// // 1) Define the "model" of our "Term" type
// //    to mimic your TypeScript union
// // ----------------------------------
// public abstract record Term
// {
// 	public record Var(string Name) : Term
// 	{
// 		public override string ToString()
// 		{
// 			return Name;
// 		}
// 	}
// 	public record Lamda(string Name, Term Body) : Term
// 	{
// 		public override string ToString()
// 		{
// 			return $"λ{Name}.{Body.ToString()}";
// 		}
// 	}
// 	public record App(Term L, Term R) : Term
// 	{
// 		public override string ToString()
// 		{
// 			return $"({L.ToString()} {R.ToString()})";
// 		}
// 	}

// 	//	public override string ToString()
// 	//	{
// 	//		return this switch
// 	//		{
// 	//			Term.Var v => v.Name,
// 	//			Term.Lamda lam => $"λ{lam.Name}.{lam.Body.ToTermString()}",
// 	//			Term.App app => $"({app.L.ToTermString()} {app.R.ToTermString()})",
// 	//			_ => throw new NotImplementedException()
// 	//		};
// 	//	}
// }

// // Helper method to pretty-print Term
// public static class TermExtensions
// {
// 	public static string ToTermString(this Term t)
// 	{
// 		return t switch
// 		{
// 			Term.Var v => v.Name,
// 			Term.Lamda lam => $"λ{lam.Name}.{lam.Body.ToTermString()}",
// 			Term.App app => $"({app.L.ToTermString()} {app.R.ToTermString()})",
// 			_ => throw new NotImplementedException()
// 		};
// 	}
// }

// // ----------------------------------
// // 2) Variables context, like your TypeScript "Variables" class
// // ----------------------------------
// public class Variables
// {
// 	public BigInteger VarsAvailable { get; }

// 	public Variables(BigInteger varsAvailable)
// 	{
// 		VarsAvailable = varsAvailable;
// 	}

// 	public int Count => (int)VarsAvailable;

// 	public string Pick(int n)
// 	{
// 		// Picks 'a' + n, e.g. 0 -> 'a', 1 -> 'b', etc.
// 		return ((char)(97 + n)).ToString();
// 	}

// 	public (string, Variables) NewVar()
// 	{
// 		var name = Pick((int)VarsAvailable);
// 		return (name, new Variables(VarsAvailable + 1));
// 	}
// }

// // ----------------------------------
// // 3) Define a "bounded function" data structure
// // ----------------------------------
// public class BoundedFunctionDef<T>
// {
// 	public int StartAt { get; set; }
// 	public int Cardinality { get; set; }
// 	public Func<int, T> F { get; set; } = default!;
// }

// // ----------------------------------
// // 4) Define the interfaces: IChoices, IChoiceBuilder, IBuiltChoices
// // ----------------------------------
// public interface IChoices<CTX, T>
// {
// 	int MaxBounded { get; }
// 	List<BoundedFunctionDef<T>> BoundedConstructors { get; }
// 	List<Func<Func<CTX, BigInteger, T>, BigInteger, T>> UnboundedConstructors { get; }
// }

// public interface IChoiceBuilder<CTX, T>
// {
// 	IBuiltChoices<CTX, T> Bud(int max, Func<int, T> f);
// 	IBuiltChoices<CTX, T> Branch(Func<Func<CTX, BigInteger, T>, BigInteger, T> f);
// }

// public interface IBuiltChoices<CTX, T> : IChoiceBuilder<CTX, T>, IChoices<CTX, T>
// {
// }

// // ----------------------------------
// // 5) The "ConstructorBuilder" interface
// //    that returns Func<BigInteger, T>
// // ----------------------------------
// public interface IConstructorBuilder
// {
// 	Func<BigInteger, T> WithContext<CTX, T>(
// 		CTX initialContext,
// 		Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> getOptions
// 	);
// }

// // ----------------------------------
// // 6) An implementation of IConstructorBuilder
// // ----------------------------------
// public class ConstructorBuilderImpl : IConstructorBuilder
// {
// 	public Func<BigInteger, T> WithContext<CTX, T>(
// 		CTX ctx,
// 		Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> getOptions
// 	)
// 	{
// 		var initialContext = ctx;

// 		return (BigInteger n) =>
// 		{
// 			// Build the choices each time we invoke
// 			var options = getOptions(
// 				initialContext,
// 				new ChoiceBuilderImpl<CTX, T>(ctx, getOptions)
// 			);

// 			BigInteger maxBounded = options.MaxBounded;
// 			if (n < maxBounded)
// 			{
// 				// Find the right bounded function
// 				int n2 = (int)n;
// 				foreach (var o in options.BoundedConstructors)
// 				{
// 					if (n2 >= o.StartAt && n2 < o.StartAt + o.Cardinality)
// 					{
// 						return o.F(n2 - o.StartAt);
// 					}
// 				}
// 				throw new Exception($"No bounded function found for n: {n}");
// 			}
// 			else
// 			{
// 				// Find the right unbounded function
// 				BigInteger length = options.UnboundedConstructors.Count;
// 				BigInteger d = n / length;
// 				BigInteger r = n % length;
// 				var f = options.UnboundedConstructors[(int)r];
// 				return f(
// 					(newContext, bigN) => this.WithContext(newContext, getOptions)(bigN),
// 					d
// 				);
// 			}
// 		};
// 	}
// }

// // ----------------------------------
// // 7) The ChoiceBuilderImpl + BuiltChoices
// // ----------------------------------
// public class ChoiceBuilderImpl<CTX, T> : IBuiltChoices<CTX, T>
// {
// 	public int MaxBounded { get; private set; } = 0;
// 	public List<BoundedFunctionDef<T>> BoundedConstructors { get; } = new();
// 	public List<Func<Func<CTX, BigInteger, T>, BigInteger, T>> UnboundedConstructors { get; } = new();

// 	// Ctor calls "initialize(ctx, this)" just like in TS
// 	public ChoiceBuilderImpl(
// 		CTX ctx,
// 		Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> initialize
// 	)
// 	{
// 		// We don't store 'initialize' results, we just call it so that
// 		// the user can register bounded/unbounded in the builder.
// 		initialize(ctx, this);
// 	}

// 	public IBuiltChoices<CTX, T> Bud(int max, Func<int, T> f)
// 	{
// 		BoundedConstructors.Add(new BoundedFunctionDef<T>
// 		{
// 			StartAt = MaxBounded,
// 			Cardinality = max,
// 			F = f
// 		});
// 		MaxBounded += max;
// 		return this;
// 	}

// 	public IBuiltChoices<CTX, T> Branch(Func<Func<CTX, BigInteger, T>, BigInteger, T> f)
// 	{
// 		UnboundedConstructors.Add(f);
// 		return this;
// 	}
// }

// // ----------------------------------
// // 8) Finally, create the "builder" instance
// // ----------------------------------
// public static class TermBuilder
// {
// 	public static IConstructorBuilder Builder { get; } = new ConstructorBuilderImpl();
// }

// // ----------------------------------
// // 9) Demonstration / Test in Main()
// //    This is the LINQPad entry point
// // ----------------------------------
// public class Program
// {
// 	public static void Main()
// 	{
// 		// The "noVariables" context
// 		var noVariables = new Variables(0);

// 		// Build our "makeTerm" function
// 		var makeTerm =
// 			TermBuilder.Builder.WithContext<Variables, Term>(
// 				noVariables,
// 				(ctx, def) =>
// 				{
// 					return def
// 						// 1) Bounded: pick from the existing variables
// 						.Bud(ctx.Count, n => new Term.Var(ctx.Pick(n)))

// 						// 2) Unbounded: create a new Var (lam abstraction)
// 						.Branch((enc, n) =>
// 						{
// 							var (name, newVars) = ctx.NewVar();
// 							return new Term.Lamda(name, enc(newVars, n));
// 						})

// 						// 3) Unbounded: application node
// 						.Branch((enc, n) =>
// 						{
// 							var l = enc(ctx, n);
// 							var r = enc(ctx, n);
// 							return new Term.App(l, r);
// 						});
// 				}
// 			);

// 		// Let's test by generating Terms for n = 0..100
// 		Enumerable.Range(1, 100)
// 			.Select(n => new
// 			{
// 				Index = n,
// 				Term = makeTerm(n).ToString()
// 			})
// 			.Dump();


// 	}
// }
