namespace GodelianToolkit.FluentBuilder;

using System;
using System.Numerics;

// ----------------------------------
// 3) Define a "bounded function" data structure
// ----------------------------------
public class BoundedFunctionDef<T>
{
    public int StartAt { get; set; }
    public int Cardinality { get; set; }
    public Func<int, T> F { get; set; } = default!;
}

// ----------------------------------
// 4) Define the interfaces: IChoices, IChoiceBuilder, IBuiltChoices
// ----------------------------------
public interface IChoices<CTX, T>
{
    int MaxBounded { get; }
    List<BoundedFunctionDef<T>> BoundedConstructors { get; }
    List<Func<Func<CTX, BigInteger, T>, BigInteger, T>> UnboundedConstructors { get; }
}

public interface IChoiceBuilder<CTX, T>
{
    IBuiltChoices<CTX, T> Bud(int max, Func<int, T> f);
    IBuiltChoices<CTX, T> Branch(Func<Func<CTX, BigInteger, T>, BigInteger, T> f);
}

public interface IBuiltChoices<CTX, T> : IChoiceBuilder<CTX, T>, IChoices<CTX, T>
{
}

// ----------------------------------
// 5) The "ConstructorBuilder" interface
//    that returns Func<BigInteger, T>
// ----------------------------------
public interface IConstructorBuilder
{
    Func<BigInteger, T> WithContext<CTX, T>(
        CTX initialContext,
        Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> getOptions
    );
}

// ----------------------------------
// 6) An implementation of IConstructorBuilder
// ----------------------------------
public class ConstructorBuilderImpl : IConstructorBuilder
{
    public Func<BigInteger, T> WithContext<CTX, T>(
        CTX ctx,
        Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> getOptions
    )
    {
        var initialContext = ctx;

        return (BigInteger n) =>
        {
            // Build the choices each time we invoke
            var options = getOptions(
                initialContext,
                new ChoiceBuilderImpl<CTX, T>(ctx, getOptions)
            );

            BigInteger maxBounded = options.MaxBounded;
            if (n < maxBounded)
            {
                // Find the right bounded function
                int n2 = (int)n;
                foreach (var o in options.BoundedConstructors)
                {
                    if (n2 >= o.StartAt && n2 < o.StartAt + o.Cardinality)
                    {
                        return o.F(n2 - o.StartAt);
                    }
                }
                throw new Exception($"No bounded function found for n: {n}");
            }
            else
            {
                // Find the right unbounded function
                BigInteger length = options.UnboundedConstructors.Count;
                BigInteger d = n / length;
                BigInteger r = n % length;
                var f = options.UnboundedConstructors[(int)r];
                return f(
                    (newContext, bigN) => this.WithContext(newContext, getOptions)(bigN),
                    d
                );
            }
        };
    }
}

// ----------------------------------
// 7) The ChoiceBuilderImpl + BuiltChoices
// ----------------------------------
public class ChoiceBuilderImpl<CTX, T> : IBuiltChoices<CTX, T>
{
    public int MaxBounded { get; private set; } = 0;
    public List<BoundedFunctionDef<T>> BoundedConstructors { get; } = new();
    public List<Func<Func<CTX, BigInteger, T>, BigInteger, T>> UnboundedConstructors { get; } = new();

    // Ctor calls "initialize(ctx, this)" just like in TS
    public ChoiceBuilderImpl(
        CTX ctx,
        Func<CTX, IChoiceBuilder<CTX, T>, IChoices<CTX, T>> initialize
    )
    {
        // We don't store 'initialize' results, we just call it so that
        // the user can register bounded/unbounded in the builder.
        initialize(ctx, this);
    }

    public IBuiltChoices<CTX, T> Bud(int max, Func<int, T> f)
    {
        BoundedConstructors.Add(new BoundedFunctionDef<T>
        {
            StartAt = MaxBounded,
            Cardinality = max,
            F = f
        });
        MaxBounded += max;
        return this;
    }

    public IBuiltChoices<CTX, T> Branch(Func<Func<CTX, BigInteger, T>, BigInteger, T> f)
    {
        UnboundedConstructors.Add(f);
        return this;
    }
}