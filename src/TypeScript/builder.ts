//  The way this builder should work is this:

interface ConstructorBuilder {
    withContext<CTX, T>(
        initialContext: CTX,
        getOptions: (ctx: CTX, d: ChoiceBuilder<CTX, T>) => Choices<CTX, T>,
    ): (n: bigint) => T;
}

type BoundedFunctionDef<T> = {
    startAt: number;
    cardinality: number;
    f: (n: number) => T;
};

interface Choices<CTX, T> {
    maxBounded: number;
    boundedConstructors: BoundedFunctionDef<T>[];
    unboundedConstructors:
        ((enc: (newContext: CTX, n: bigint) => T, n: bigint) => T)[];
}

interface ChoiceBuilder<CTX, T> {
    addBounded(max: number, f: (n: number) => T): BuiltChoices<CTX, T>;
    addUnbounded(
        f: (enc: (newContext: CTX, n: bigint) => T, n: bigint) => T,
    ): BuiltChoices<CTX, T>;
}

interface BuiltChoices<CTX, T> extends ChoiceBuilder<CTX, T>, Choices<CTX, T> {
}

class ConstructorBuilderImpl implements ConstructorBuilder {
    withContext<CTX, T>(
        ctx: CTX,
        getOptions: (ctx: CTX, d: ChoiceBuilder<CTX, T>) => Choices<CTX, T>,
    ): (n: bigint) => T {
        const initialContext = ctx;

        return (n: bigint) => {
            const options = getOptions(
                initialContext,
                new ChoiceBuilderImpl(ctx, getOptions),
            );
            const maxBounded = BigInt(options.maxBounded);
            if (n < maxBounded) {
                //  Find the right bounded function and call it
                const n2 = Number(n);
                for (const o of options.boundedConstructors) {
                    if (n2 >= o.startAt && n2 < o.startAt + o.cardinality) {
                        return o.f(n2 - o.startAt);
                    }
                }
                //  This should never happen, but throw a useful error if it does
                throw new Error("No bounded function found for n: " + n);
            } else {
                //  Find the right unbounded function and call it
                const length = BigInt(options.unboundedConstructors.length);
                const d = n / length;
                const r = n % length;
                const f = options.unboundedConstructors[Number(r)];
                return f(
                    (newContext, n) =>
                        this.withContext(newContext, getOptions)(n),
                    d,
                );
            }
        };
    }
}

class ChoiceBuilderImpl<CTX, T> implements BuiltChoices<CTX, T> {
    public maxBounded = 0;
    public boundedConstructors: BoundedFunctionDef<T>[] = [];
    public unboundedConstructors:
        ((enc: (newContext: CTX, n: bigint) => T, n: bigint) => T)[] = [];

    constructor(
        ctx: CTX,
        initialize: (ctx: CTX, d: ChoiceBuilder<CTX, T>) => void,
    ) {
        initialize(ctx, this);
    }

    addBounded(max: number, f: (n: number) => T): BuiltChoices<CTX, T> {
        this.boundedConstructors.push({
            startAt: this.maxBounded,
            cardinality: max,
            f,
        });
        this.maxBounded += max;
        return this;
    }

    addUnbounded(
        f: (enc: (newContext: CTX, n: bigint) => T, n: bigint) => T,
    ): BuiltChoices<CTX, T> {
        this.unboundedConstructors.push(f);
        return this;
    }
}

export const builder: ConstructorBuilder = new ConstructorBuilderImpl();
