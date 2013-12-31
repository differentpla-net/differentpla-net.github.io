1. If it's in the cache, return it.
2. If it's not in the cache, ask the gen_server to create it and put it in the cache. gen_server returns it from the cache.
3. If the gen_server finds that it's in the cache -- someone else raced us; return it from the cache ourselves.
4. creating it: use a poolboy pool of compilers.


But: you can't simply use a compositional cache. Consider this:

- I use cache:get(Key, Fun)
    - There's a race. Either I end up calling Fun multiple times, either in the caller, or in the worker pool. This is wasted work.
    - So: I need to ensure that at most _one_ worker/caller is working on 'Key' at a time. Without some smarts to reserve/return 'Key', I then end up with the degenerate case of a single worker for all keys. This leads to work not getting done.
    So: I need to peek in the cache. If there, return it. If not, _then_ we drop down to a single worker. No: same problem.
    If we drop down to multiple workers, other same problem.

    How about:

    1. peek in the cache. resolved? OK.
    resolving? wait for the response. TODO: How to subscribe to this?
    missing? ask a worker to resolve it. Mark it as resolving. Wait for the response.

    Ridiculously complicated and hard to get right. Surely someone's already done it.

    How about: if it's missing, ask a worker to resolve it. If the worker discovers that another worker is working on it, then it -- nah. Still got race conditions.

    You basically need a cache of promises. These can then be resolved once, but lots of people can block on them.

