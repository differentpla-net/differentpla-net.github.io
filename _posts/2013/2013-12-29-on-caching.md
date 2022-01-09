---
title: On caching
date: 2013-12-29T09:12:01Z
---
## Background

There's a quote:

> "There are only two hard problems in Computer Science: cache invalidation and
> naming things."
>
> -- Phil Karlton

I'm going to talk about caching, but I'm *not* going to talk about cache invalidation. Instead, I'm going to talk about
caching in the face of concurrent queries.

I'm thinking about this right now, because I'm looking at one of [electric imp's](http://electricimp.com) backend
daemons; specifically, the one that handles incoming device connections.

When this daemon restarts (after an upgrade, say), all of the imps that were connected to it reconnect immediately. This
reconnection is transparent to user code, but it results in a "stampede" of imps hitting the daemon in one go.

Now, obviously, we cache a lot of the database queries, but when the daemon has recently restarted, these caches will be
cold, and they'll be getting hit by the stampede.

## Threads and Caches

This particular backend daemon at Electric Imp is written in Erlang, but I'll use C# for my examples, since it's more
accessible.

Let's assume that I've got the following C# code:

```c#
var cache = new ConcurrentDictionary<int, int>();
var value = cache.GetOrAdd(42, k => ExpensiveCalculation(k));
```

This is perfectly safe when used from multiple threads, but there's a problem with the amount of work that's done.
Consider what happens if we call it like this:

```c#
const int THREAD_COUNT = 1000;
Task.WaitAll(
    Enumerable.Range(1, THREAD_COUNT)
                .Select(i =>
                        Task.Factory.StartNew(
                        () => {
                            var value = cache.GetOrAdd(42, k => ExpensiveCalculation(k));
                            // do something with value.
                        })
                ).ToArray();
);
```

That is: we spawn 1,000 threads, each of which queries the cache for the same key.

What actually happens is that more than one thread gets to run. Each of the running threads discovers that the cache
doesn't contain the key, and begins the expensive calculation. This means that instead of happening once, our expensive
calculation happens *N* times.

In this case, *N* happens to be roughly 4, because that's the number of logical cores in my laptop. This could easily
get much, much, worse if -- instead of performing some calculation -- you were hitting a database, and you did it
asynchronously, returning those tasks to the pool until the results came back.

## I know, let's use a lock

> "Some programmers, confronted with a threading problem, think "I know, I'll
> use a lock". Now... They... Have... A... Different... Problem..."
>
> -- Me, just now.

So, if the problem is that we don't want to call `ExpensiveCalculation` more than once at a time, we can use a lock:

```c#
int LockedExpensiveCalculation(int value)
{
    lock(something)
    {
        Thread.Sleep(1000);
        return value * value;
    }
}
```

That's gotta be better, right?

Well, actually, no. This time, the threads race each other to grab the lock, meaning that, instead of *N* threads
running the function in parallel, they end up convoying on the lock, and they take it in turns to run the function. This
means extra delay on top of the unneeded work.

## But the lock's in the wrong place!

Well, I'm stupid. The reason that the threads are convoying on the lock is
because we put it inside the cache miss handler. Let's put it around the cache
query instead:

```c#
// ...
Task.Factory.StartNew(() => {
    int value;
    lock(something)
    {
        value = cache.GetOrAdd(42, k => ExpensiveCalculation(k));
    }
});
```

Is that any better?

Well, for certain values of "better", yes. We only run the expensive calculation once, meaning less work. In this
contrived example, there's also negligible delay over the naive case as well, because the other threads just wait until
the cache contains the required value.

Let's contrive a slightly more complicated example.

## A slightly more complicated contrived example

In this example, the threads aren't all asking for the same value. Instead, they're asking for a mixture of values:

```c#
value = cache.GetOrAdd(i % 8, k => ExpensiveCalculation(k));
```

Now the problem is that we end up blocking each thread, even if it's not their expensive calculation that they're
waiting for. Now, depending on the nature of the calculation, this might make sense.

But, if we do have multiple cores (or a beefy database server), it makes sense to let at least some of the threads
execute in parallel.

## Promises

What we need for that are **promises** or **futures**. In .NET-land, these are represented (more-or-less) by the
`Task<T>` and `TaskCompletionSource<T>` types.

Instead of caching the actual value, we cache a promise:

```c#
var cache = new ConcurrentDictionary<int, Task<int>>();
var value = cache.GetOrAdd(i % 8, k => PromiseExpensiveCalculation(k));
```

Where:

```c#
Task<int> PromiseExpensiveCalculation(int k)
{
    var completion = new TaskCompletionSource<int>();
    Task.Factory.StartNew(() => completion.SetResult(ExpensiveCalculation()));
    return completion.Task;
}
```

This immediately returns a promise to the calling threads, with one promise for each value of the key. This prevents the
calling threads from all calling `ExpensiveCalculation` at once.

It also spawns another task to actually do the expensive calculation. When this is completed, it will fulfill the
promise, releasing the waiting threads.

We can then use the .NET task scheduling algorithm to ensure that there aren't too many threads calling
`ExpensiveCalculation` at the same time.

*Cough*. Yeah. About that. In order to get this to work, you'll need to pass `TaskCreationOptions.LongRunning` to
`Task.Factory.StartNew()`, otherwise .NET will prove remarkably reluctant to start the worker thread. Which means you'll
make no progress.

That's a documented thing, but you need to be aware of it.

## Queue

Alternatively, you could come up with an implementation where you have a pool of worker threads, a dictionary of
`Key -> Value | Semaphore` (basically a promise), and a queue of requests. It's equivalent to the above, but you get more
control over what's going on.

## What about async/await?

Yeah, probably.

## Conclusions

I hope that I've demonstrated that:

1. Naive cache access patterns can result in more work being done than necessary.
2. Locking can result in excessive slowdowns, loss of concurrency, and -- if done wrong -- can still result in
   unnecessary work being done.
3. Promises (or some equivalent) are the right answer.
