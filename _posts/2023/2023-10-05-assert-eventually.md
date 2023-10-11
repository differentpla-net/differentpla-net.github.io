---
title: "assert:eventually"
date: 2023-10-05T07:49:00.000Z
tags: erlang
---

Context: something's happening asynchronously in the background (maybe you're waiting for your web server to finish
starting). Need to wait for (or poll for) something to happen. So you want to write something like this:

Here's some Python to serve as pseudo-code:

```python
def wait_for_server():
    for retry in range(MAX_RETRIES):
        if is_server_available():
            return True
        else:
            time.sleep(1)
    return False

assert wait_for_server()
```

In Erlang, it might look like this:

```erlang
wait_for_server() ->
    wait_for_server(?MAX_RETRIES).

wait_for_server(Retry) when Retry > 0 ->
    case is_server_available() of
        true ->
            true;
        false ->
            timer:sleep(1_000),
            wait_for_server(Retry - 1)
    end;
wait_for_server(_Retry) ->
    false.

?assert(wait_for_server()).
```

But how do we generalise it? We want something like this:

TODO

What if we want to check, say, the number of active connections to the server?

TODO: demonstrate how awkward that is if we need to check different numbers. This motivates parameterising the probe/condition.

TODO: motivate separating the probe/condition somehow. Oh, yeah: because the probe might be useful in a synchronous situation -- such as getting the number of active connections.

TODO: How could this integrate with erlang-hamcrest?

TODO: motivate wait_for/wait_until vs. assert_eventually.

Note that we can treat an exception as a failure, and retry (depending on options?). This simplifies the probe, since it
can "let it crash".

TODO: How could you do it with events, instead? Let's say you had server-sent events, or Erlang messages.

It's nice that this is readable:

```erlang
    wait_for(is_topic_ready(ClusterId, Topic)),
```

It's not so nice that we have to remember to return a function as the probe:

```erlang
is_topic_ready(ClusterId, Topic) ->
    fun() ->
        {ok, _} = kafire_metadata:partitions(ClusterId, Topic),
        true
    end.
```

Is there a way we could wrap this in a macro, to add the fun() bits?

Look at meck for its syntactic sugar; look at hamcrest (both Erlang and Python).

Naming: at imp, we used Probe/Condition. I just saw a post on the socials where someone (Quick/Nimble, a testing thing
for Swift, inspired by Cedar) changed Predicate to Matcher. Is 'Matcher' a useful difference from 'Condition'?

What about logging? Do we want to sprinkle ?LOG_DEBUG inside the loop, so we can see what the probe/matcher return on
each iteration? Does that need a bunch of macros, so we can ??Expr them...?
