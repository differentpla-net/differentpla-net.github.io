---
title: "Using Expression<T> as a Compiler, to avoid writing conversion code"
date: 2009-09-03T15:22:33.000Z
x-drupal-nid: 238
---
Marc Gravell wrote about [using `Expression<T>` as a compiler](http://www.infoq.com/articles/expression-compiler). It was a bit of an eye-opener.

There's a bit in it where he says:

> A similar approach [to using Expression for shallow cloning an object] might be used, for example, to map data between DTO entities and entity objects...

Say no more:

```c#
static class Conversion<TInput, TOutput>
{
    private static readonly Func<TInput, TOutput> Converter;

    static Conversion()
    {
        Converter = CreateConverter();
    }

    public static Func<TInput, TOutput> CreateConverter()
    {
        var input = Expression.Parameter(typeof(TInput), "input");

        // For each property that exists in the destination object,
        // is there a property with the same name in the source object?

        // Get a list of writable properties on the destination.
        var destinationProperties = typeof(TOutput)
            .GetProperties(BindingFlags.Public | BindingFlags.Instance)
            .Where(prop => prop.CanWrite);

        // Get a list of readable properties on the source.
        var sourceProperties = typeof(TInput)
            .GetProperties(BindingFlags.Public | BindingFlags.Instance)
            .Where(prop => prop.CanRead);

        // Where the named property exists in both, generate a 'bind' (assignment) expression.
        var memberBindings = sourceProperties.Join(destinationProperties,
            sourceProperty => sourceProperty.Name,
            destinationProperty => destinationProperty.Name,
            (sourceProperty, destinationProperty) =>
                (MemberBinding)Expression.Bind(destinationProperty,
                    Expression.Property(input, sourceProperty)));

        // Generate a member initializer containing those bindings.
        var body = Expression.MemberInit(Expression.New(typeof(TOutput)), memberBindings);

        // Turn it into a function and compile it.
        var lambda = Expression.Lambda<Func<TInput, TOutput>>(body, input);
        return lambda.Compile();
    }

    public static TOutput From(TInput input)
    {
        return Converter(input);
    }
}
```

Use it like this:

```c#
CustomerDto customerDto = CustomerService.GetCustomerById(1);
Customer customer = Conversion<CustomerDto, Customer>.From(customerDto);
```

I also threw a fluent interface together as well...

```c#
static class Conversion
{
    internal class ConversionFrom<TInput>
    {
        private readonly TInput _input;

        public ConversionFrom(TInput input)
        {
            _input = input;
        }

        public TOutput To<TOutput>()
        {
            return Conversion<TInput, TOutput>.From(_input);
        }
    }

    internal class ConversionTo<TOutput>
    {
        public TOutput From<TInput>(TInput input)
        {
            return Conversion<TInput, TOutput>.From(input);
        }
    }

    public static ConversionFrom<TInput> From<TInput>(TInput input)
    {
        return new ConversionFrom<TInput>(input);
    }

    public static ConversionTo<TOutput> To<TOutput>()
    {
        return new ConversionTo<TOutput>();
    }
}
```

Use it like this:

```c#
Customer customer = Conversion.From(customerDto).To<Customer>();
```

...or like this:

```c#
Customer customer = Conversion.To<Customer>().From(customerDto);
```
