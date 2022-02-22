---
title: Debugging gen_server timeouts
date: 2019-09-21 15:01
tags: erlang
---

When an error occurs in the [Electric Imp](https://electricimp.com) backend, it's logged and collated, and we regularly
review error reports to see if there's anything that needs looking at.

Occasionally we'd see an error that looked something like this (I've ellided logging metadata and a bunch of the call
stack):

```
2019-09-19 21:18:09.761 imp_server@node [error] ... <0.26971.2508>@imp_protocol:call_handler:247 timeout exit at [{gen_server,call,2,[{file,"gen_server.erl"},{line,215}]}, ...}]
2019-09-19 21:18:09.762 imp_server@node [error] <0.26971.2508> gen_server <0.26971.2508> terminated with reason: timeout in gen_server:call/2 line 215
2019-09-19 21:18:09.763 imp_server@node [error] <0.26971.2508> CRASH REPORT Process <0.26971.2508> with 0 neighbours exited with reason: timeout in gen_server:call/2 line 215
2019-09-19 21:18:09.764 imp_server@node [error] <0.30948.2518> gen_server <0.30948.2518> terminated with reason: timeout
2019-09-19 21:18:09.764 imp_server@node [error] <0.30948.2518> CRASH REPORT Process <0.30948.2518> with 0 neighbours exited with reason: timeout in gen_server:decode_msg/9 line 432
2019-09-19 21:18:09.764 imp_server@node [error] <0.7906.2452> gen_fsm <0.7906.2452> in state ready terminated with reason: timeout
2019-09-19 21:18:09.764 imp_server@node [error] <0.7906.2452> CRASH REPORT Process <0.7906.2452> with 0 neighbours exited with reason: timeout in gen_fsm:terminate/7 line 609
2019-09-19 21:18:09.764 imp_server@node [error] <0.26971.2508> Ranch listener '...' terminated with reason: timeout
```

At Electric Imp, we offer a secure-by-default Internet-of-Things platform. One of our features is that each IoT device
has a corresponding "agent" running in our cloud, so that the device need not remain connected (good for battery life).

The error was caused by a timeout in a call from the device connection process to the agent process. Calls use
`gen_server:call`, which has a default timeout of 5 seconds.

This was only happening in our production environment, and only once or twice a month. The particular call was
idempotent, and the calling process would restart anyway (because this is Erlang).

So, other than occasionally having another peek at the relevant code (which wasn't yielding any obvious clues), we
didn't assign a high priority to the error report.

This week, the error started occurring a few times a day, and always for the same connected device. So I started to dig
in.

When you see a `timeout` exception from `gen_server:call`, it often means that a particular `handle_call` clause is
taking more than 5 seconds (the default timeout) to reply. Inspecting the source code had me fairly convinced that all
of our `handle_call` clauses were OK. 5 seconds is a long time, and I couldn't see how any of them could be taking
_that_ long.

Another potential cause is that the `gen_server` is processing a lot of messages, and the `'$gen_call'` message from
`gen_server:call` is being held up in the message queue.

I [asked](http://erlang.org/pipermail/erlang-questions/2019-September/098444.html) on the Erlang mailing list to see if
anyone could suggest a way (preferably built-in) to trigger an alert if a `gen_server` didn't meet its "SLA" for
`handle_call`. There appears to be nothing built in, but Jesper
[suggested](http://erlang.org/pipermail/erlang-questions/2019-September/098446.html) modifying the caller to call
`erlang:process_info/2` to find out what the callee was doing if it timed out. Max [followed up](http://erlang.org/pipermail/erlang-questions/2019-September/098447.html) with some code lifted
from their product which extracts the stack trace and a few other things.

So I modified Max's code (which was specific to their product) and hotpatched the Erlang node to dump the callstack and
the current message queue if we saw another timeout exception. Then I went to the gym (one of the benefits of working
remotely) while I waited for it to happen again.

Unfortunately, I forgot to mention that the calling process and called process are in different Erlang nodes, and I
missed the note in the documentation that says that `erlang:process_info/2` doesn't work on non-local processes. So,
when I got back from the gym, I discovered a _different_ set of errors in the logs, complaining about the misuse of
`process_info`.

Siraaj and Aleksey had suggested modifying the called process to wrap `handle_call` to alert if the SLA wasn't met, but
I felt that this would be a little invasive on a production server. It's something we'll be looking at in future,
though.

But: based on the idea that I'd want to record the entry and exit timestamps for `handle_call`, I decided to reach for
Erlang's `dbg` module, which allows you to be notified when a function is called or returns, and I came up with
something which I could apply in an Erlang remote console. I'll break it down.

First, we define our trace function. This will get called whenever something interesting happens. It looks like this:

```erlang
%% TState :: map(Fun -> [{Time, Arg0}]).
TFun =
    fun(_Msg = {trace, _Pid, call, {Mod, Fun, [Arg0 | _]}}, TState) ->
        % push an entry timestamp.
        Call = {erlang:monotonic_time(), Arg0},
        maps:update_with(Fun, fun(Calls) -> [Call | Calls] end, [Call], TState);

       (_Msg = {trace, _Pid, return_from, {Mod, Fun, _Arity}, _Result}, TState) ->
        % pop entry timestamp.
        [{Then, Arg0} | Calls] = maps:get(Fun, TState),
        AssertElapsed(Pid, Fun, Arg0, Then),
        maps:update(Fun, Calls, TState)
    end.
```

When it sees a `call` message, it pushes the current timestamp, and when it sees a `return_from` message, it pops that
timestamp and asserts that not too much time has elapsed. It assumes that you've already defined `AssertElapsed` (see
below).

The calls are recorded in a map keyed by the function name. This is because any of the `gen_server` callbacks could
block message processing, so we need to record `Mod:handle_call`, `Mod:handle_info` and `Mod:handle_cast` separately.

Each call has a stack of entry timestamps, because in some modules you'll have `handle_call` call itself. It also
records the first argument so that we can tell the difference between the various `handle_call` and `handle_info`
clauses.

`AssertElapsed` looks like this:

```erlang
AssertElapsed =
    fun(Pid, Call, Req, Then) ->
        Now = erlang:monotonic_time(),
        ElapsedMicros = erlang:convert_time_unit(Now - Then, native, microsecond),
        if
            ElapsedMicros > 5000 ->
                io:format("~s ~p(~p) took ~B us\n",
                    [lists:flatten(ec_date:format("c", calendar:universal_time())), Call, Req, ElapsedMicros]);
            true ->
                ok
        end,
        {message_queue_len, QueueLen} = erlang:process_info(Pid, message_queue_len),
        if
            QueueLen > 5 ->
                io:format("~s ~p\n", [lists:flatten(ec_date:format("c", calendar:universal_time())), erlang:process_info(Pid, messages)]);
            true ->
                ok
        end
    end.
```

It asserts two things: that the elapsed time between entering and leaving the function was less than 5ms, and that there
are no more than 5 messages in the queue. It uses `ec_date:format` (from Erlware Commons) for formatting the time.

Now all we need to do is hook it up:

```erlang
Mod = ...
Pid = ...

dbg:start().
dbg:tracer(process, {TFun, TState0}).
dbg:tpl(Mod, handle_call, '_', dbg:fun2ms(fun(_) -> return_trace() end)).
dbg:tpl(Mod, handle_info, '_', dbg:fun2ms(fun(_) -> return_trace() end)).
dbg:tpl(Mod, handle_cast, '_', dbg:fun2ms(fun(_) -> return_trace() end)).
dbg:p(Pid, c).
```

Note that I was careful to trace only a few functions and only a single process. Remember: this is a production server.

I came back a few hours later after dinner to find the following (details ellided) in my console:

```
2019-09-20 20:32:06 handle_info({log, ...}) took 30121427 us
```

30 seconds. Hmmm. We're spending so long in `handle_info` that a `gen_server:call` immediately afterward times out.

But now I had a specific place to start digging.

After all that, the conclusion is a little underwhelming: we had a hand-written `list_join` function, dating from when
we were targetting Erlang/OTP R16. For a particular degenerate class of input, the performance was ... not good.
Replacing it with `lists:join/2` (introduced in OTP-19.0) results in a call that previously took 30 seconds now taking
~120 milliseconds. I think I'll call that an improvement.
