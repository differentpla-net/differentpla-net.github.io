---
title: "LINQ's ForEach doesn't work on IEnumerable<T>"
date: 2009-03-29T09:39:45.000Z
---
For some reason, LINQ's ForEach extension method doesn't work on `IEnumerable<T>`; it only works on `IList<T>`. Easy fix:

```c#
public static class EnumerableExtensions
{
    public static void ForEach<T>(this IEnumerable<T> values, Action<T> action)
    {
        foreach (var value in values)
        {
            action(value);
        }
    }
}
```
