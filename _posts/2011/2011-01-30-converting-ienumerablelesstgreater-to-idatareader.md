---
title: "Converting IEnumerable<T> to IDataReader"
date: 2011-01-30T15:09:06.000Z
redirect_from: /content/2011/01/converting-ienumerablet-idatareader
---
If you’ve got an enumerable, and you want to pass it to SqlBulkCopy, you can turn it into an IDataReader. Something like the following might be useful:

```
static class DataReaderExtensions
{
    public static IDataReader AsDataReader<TSource>(this IEnumerable<TSource> source, int fieldCount, Func<TSource, int, object> getValue)
    {
        return EnumerableDataReader.Create(source, fieldCount, getValue);
    }
}

internal static class EnumerableDataReader
{
    public static IDataReader Create<TSource>(IEnumerable<TSource> source, int fieldCount, Func<TSource, int, object> getValue)
    {
        return new EnumerableDataReader<TSource>(source.GetEnumerator(), fieldCount, getValue);
    }
}

internal class EnumerableDataReader<TSource> : IDataReader
{
    private readonly IEnumerator<TSource> _source;
    private readonly int _fieldCount;
    private readonly Func<TSource, int, object> _getValue;

    internal EnumerableDataReader(IEnumerator<TSource> source, int fieldCount, Func<TSource, int, object> getValue)
    {
        _source = source;
        _getValue = getValue;
        _fieldCount = fieldCount;
    }

    public void Dispose()
    {
        // Nothing.
    }

    public object GetValue(int i)
    {
        return _getValue(_source.Current, i);
    }

    public int FieldCount
    {
        get { return _fieldCount; }
    }

    public bool Read()
    {
        return _source.MoveNext();
    }
}
```

Note that there are several more methods on IDataReader that you’ll need to implement. Fortunately, they never seem to be called in this scenario, so I had them all throw NotImplementedException().
