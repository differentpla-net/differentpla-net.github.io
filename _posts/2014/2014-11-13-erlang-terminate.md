---
title: When does terminate get called?
date: 2014-11-13 13:22:12
tags: erlang
---

In Erlang, in a `gen_server`, when *does* terminate get called? Also, some
messing around with `dbg` for tracing.

**tl;dr: if your process is trapping exits, `terminate` is called for
everything except `exit(Pid, kill)`.**

## Boilerplate

Let's start with a simple `gen_server`. See [this
gist](https://gist.github.com/rlipscombe/6824460d204adce433ca) for the
boilerplate.

Then, in the Erlang shell:

    1> c(example_server).
    {ok, example_server}
    2> {ok, Pid} = example_server:start_link().
    {ok, <0.48.0>}
    3> exit(Pid, kill).
    ** exception exit: killed

There's nothing surprising there: we sent an exit signal (with `kill`) to the
process. It died, which killed the shell process.

## Tracing

In order to see if `terminate` _is_ called, we're going to either need some
tracing (via `dbg`) or some logging (which means writing some code).

*Roger flips a coin*

Tracing it is. So, starting from the beginning, we have:

    1> c(example_server).
    {ok, example_server}
    2> dbg:start().
    {ok,<0.41.0>}
    3> dbg:tracer().
    {ok,<0.41.0>}
    4> dbg:tp(example_server, []).
    {ok,[{matched,nonode@nohost,10}]}
    5> dbg:p(all, c).
    {ok,[{matched,nonode@nohost,26}]}
    6> {ok, Pid} = example_server:start_link().
    (<0.33.0>) call example_server:start_link()
    (<0.47.0>) call example_server:init([])
    {ok,<0.47.0>}

We can turn on a bunch more tracing for that process with the following.

    7> dbg:p(Pid, [c,m,p]).

Do **not** specify `all` for the process here...

And we can see that's working, because:

    8> Pid ! hello.
    (<0.47.0>) << hello
    (<0.47.0>) call example_server:handle_info(hello,undefined)
    hello

And, if we send it an exit signal, specifying `normal`:

    9> exit(Pid, normal)
    true

OK, let's try it the hard way:

    10> exit(Pid, kill).
    (<0.47.0>) exit killed
    (<0.47.0>) unregister example_server

## `process_flag`

But `terminate` didn't get called, because we're **not trapping exits**. See
[`process_flag`](http://erlang.org/doc/man/erlang.html#process_flag-2). To trap
exits in a `gen_server`:

    init([]) ->
        process_flag(trap_exit, true),
        State = undefined,
        {ok, State}.

If we're trapping exits, we see, for example:

    9> exit(Pid, normal).
    (<0.48.0>) << {'EXIT',<0.33.0>,normal}
    (<0.48.0>) call example_server:terminate(normal,undefined)
    (<0.48.0>) exit normal
    (<0.48.0>) unregister example_server
    true

## Exit reasons

What about other exit reasons?

 - `exit(Pid, normal)` calls `terminate(normal, State)`; no exception.
 - `exit(Pid, shutdown)` calls `terminate(shutdown, State)`; exception.
 - `exit(Pid, {shutdown, whoops})` calls `terminate({shutdown, whoops}, State)`; exception.
 - `exit(Pid, computer_says_no)` calls `terminate(computer_says_no, State)`; exception.
 - `exit(Pid, kill)` does not call `terminate/2`; exception.

What's going on here is that:

 1. The exit signals (apart from `kill`) are converted to `{'EXIT', From,
    Reason}` messages.
 2. The `gen_server` module [handles the `'EXIT'`
    messages](https://github.com/erlang/otp/blob/OTP-17.3/lib/stdlib/src/gen_server.erl#L380)
    and [calls
    `Mod:terminate`](https://github.com/erlang/otp/blob/OTP-17.3/lib/stdlib/src/gen_server.erl#L721),
    passing the reason.
 3. After `Mod:terminate` has returned, the original reason is passed to
    `exit/1`, which is propagated to linked processes (usually a supervisor of
    some sort).

## Shutdown

OK, so what if the process itself chooses to stop? This is a `gen_server`, so
it's allowed to return `{stop, NewState}` from the callback functions.

Add these clauses to the top of `handle_call`:

    handle_call(stop, _From, State) ->
        {stop, normal, ok, State};
    handle_call({stop, Reason}, _From, State) ->
        {stop, Reason, ok, State};

Add these clauses to the top of `handle_info`:

    handle_info(stop, State) ->
        {stop, normal, State};
    handle_info({stop, Reason}, State) ->
        {stop, Reason, State};

And, now, we can poke it with, for example:

    ok = gen_server:call(example_server, {stop, hammer_time}).

## Errors

What about if the process runs into a missing function clause? Or if it calls
`exit/1` itself?

Yes, `terminate` is still called.

## Conclusion

At this point, I ran out of things to try, and I've come to the conclusion
that, provided you're trapping exits, `terminate` will be called for everything
except `exit(Pid, kill)`.
