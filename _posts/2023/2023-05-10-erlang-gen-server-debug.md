---
title: "gen_server debug options"
date: 2023-05-10T10:22:00.000Z
tags: erlang
---

Erlang's `gen_server` (and `gen_statem`, etc.) allow for you to pass `{debug, DebugOptions}` in
`gen_server:start_link/4` (et al.). The documentation for it is spread over several pages, making it hard to understand
at first. How should we use it?

Ordinarily when you start a `gen_server`, you'll do it something like this:

```erlang
-module(example_server).
-export([start_link/0]).
% etc.

start_link(Host, Port) ->
    % Host, Port are just example names; we're not going to use them for anything.
    gen_server:start_link(?MODULE, [Host, Port], []).
```

That empty list `[]` is the "start options" for the `gen_server`. The `start_opt()` type is documented [here](https://www.erlang.org/doc/man/gen_server.html#type-start_opt). The one we're interested in is this one:

```erlang
{debug, Dbgs :: [sys:debug_option()]}
```

`sys:debug_option()` is defined as follows:

```erlang
debug_option() =
    trace | log |
    {log, N :: integer() >= 1} |
    statistics |
    {log_to_file, FileName :: file:name()} |
    {install,
     {Func :: dbg_fun(), FuncState :: term()} |
     {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}}
```

<div class="callout callout-info" markdown="span">
The `FileName` in `{log_to_file, FileName}` is relative to the current working directory. You should use a fully-qualified name.
</div>

The documentation says:

> For every entry in Dbgs, the corresponding function in sys(3) is called.

I initially read this as meaning "the corresponding function [...] is called _with the debug event_. That is: when
`gen_server` wants to emit a debug event, it will call (e.g.) `sys:trace`. This is **not** how it works.

What it means is that this...

```erlang
{ok, Pid} = gen_server:start_link(?MODULE, [], [{debug, [trace, {log_to_file, "debug.log"}]}]).
```

...is the same as this:

```erlang
{ok, Pid} = gen_server:start_link(?MODULE, [], []),
sys:trace(Pid, true),
sys:log_to_file(Pid, "debug.log").
```

That's not strictly true -- it doesn't actually call those functions -- but it does the equivalent.

## `trace` and `log_to_file`

Essentially, what it's saying is that if you do this:

```erlang
start_link(Host, Port) ->
    Dbgs = [trace, {log_to_file, "debug.log"}],
    gen_server:start_link(?MODULE, [Host, Port], [{debug, Dbgs}]).
```

...then, at appropriate points, `gen_server` will write debug events to stdout and to the named file. It will look
something like this:

```
1> {ok, Pid} = example_server:start_link().
{ok,<0.183.0>}
2> gen_server:call(Pid, hello).
*DBG* <0.183.0> got call hello from <0.176.0>
ok
*DBG* <0.183.0> sent ok to <0.176.0>, new state {state}
```

The file `debug.log` contains the following:

```
*DBG* <0.183.0> got call hello from <0.176.0>
*DBG* <0.183.0> sent ok to <0.176.0>, new state {state}
```

That's `trace` and `log_to_file` covered. What about some of the others?

## `log`

If you specify `{debug, [log]}`, then `gen_server` records the events internally. You can retrieve them by using `sys:log/2`:

```
3> {ok, Events} = sys:log(Pid, get).
{ok,[{in,{'$gen_call',{<0.168.0>,
                       [alias|#Ref<0.755885323.652279811.247301>]},
                      hello}},
     {out,ok,
          {<0.168.0>,[alias|#Ref<0.755885323.652279811.247301>]},
          {state}}]}
4> sys:log(Pid, print).
*DBG* <0.175.0> got call hello from <0.168.0>
*DBG* <0.175.0> sent ok to <0.168.0>, new state {state}
ok
```

You can also see them in the output from `sys:get_status/1`, or in Observer's process window, in the "State" tab.

## `statistics`

This collects various, well, statistics about the process:

{% raw %}
```
10> sys:statistics(Pid, true).
ok
11> gen_server:call(Pid, hello).
ok
12> sys:statistics(Pid, get).
{ok,[{start_time,{{2023,5,10},{13,26,36}}},
     {current_time,{{2023,5,10},{13,27,0}}},
     {reductions,101},
     {messages_in,1},
     {messages_out,1}]}
```
{% endraw %}

You can see that it records the following:

- `start_time`: when statistics collection was turned on.
- `current_time`: the current time; you can use this with the above to work out how long statistics collection has been
  enabled for.
- `reductions`: the number of "reductions" that have been run while statistics collection has been enabled. Reductions
  are a count of how many function calls have happened; they're a rough estimate of how much work the process has done.
- `messages_in` and `messages_out` are a count of how many messages have been sent and received through the `gen_server` mechanisms. They do not count `OtherPid ! Message` from the `gen_server` process.

## `install`

This allows you to install your own debugging function. For example, enter the following in the console:

```erlang
DebugFun = fun(DebugState, Event, ProcState) ->
    io:format("~p ~p\n", [Event, ProcState]),
    case DebugState of
        10 -> done;
        Count -> Count + 1
    end
end.

{ok, Pid} = example_server:start_link().
sys:install(Pid, {DebugFun, 1}).
```

This installs a debug function that prints out the first ten debug events and then is automatically removed.
