---
title: "Generating the Fibonacci sequence by using the yield keyword, non-recursively"
date: 2007-06-03T15:21:14.000Z
redirect_from: /content/2007/06/generating-the-fibonacci-sequence-by-using-the-yield-keyword-non-recursively
---

```
class FibonacciSequence : IEnumerable
{
    public IEnumerator GetEnumerator()
    {
        yield return (long)0;
        yield return (long)1;

        long prev = 0;
        long curr = 1;

        for (; ; )
        {
            long next = prev + curr;
            if (next < 0)   // It overflowed. Stop.
                yield break;

            yield return next;
            prev = curr;
            curr = next;
        }
    }
}
```

Call it like this:

```
FibonacciSequence f = new FibonacciSequence();

foreach (long n in f)
    Console.WriteLine(n);
```
