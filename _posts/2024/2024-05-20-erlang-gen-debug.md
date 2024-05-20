---
title: "gen_server/gen_statem: redirecting tracing to logger"
date: 2024-05-20T11:05:00Z
tags: erlang
---

In an [earlier post]({% post_url 2023/2023-05-10-erlang-gen-server-debug %}), I talked about how you can turn on `gen_server` and `gen_statem` debug tracing by adding (e.g.) `[{debug, trace}]` to the start options. I recently discovered a way to integrate this with Erlang's default `logger`.

## Why would you want to do this?

Using `[{debug, trace}]` is great, but it has a few problems:

- It's not particularly fine-grained -- it's either enabled or disabled.
- Unlike `logger`, output goes to the terminal; it's hard to send it to (e.g.) a file.
- You can't attach logging metadata to it.

## Integrating with `logger`

It occurred to me that one of the debug options is `install`, which you can use as follows, for example:

```erlang
start_link() ->
    gen_statem:start_link(?MODULE, [],
        [
            {debug, [
                {install, {?MODULE, {fun tracer/3, undefined}}}
            ]}
        ]).
```

...and then `tracer/3` can look like this:

```erlang
% for gen_statem, StateData is the pid or registered name.
tracer(TraceState = undefined, Event, StateData) ->
    logger:log(debug, "~tp", [Event]),
    TraceState.
```

## Logging Metadata

One useful thing about `logger` is that you can attach logging metadata to each process, and that can be used by the log formatter. This is mostly automatic, but where it becomes useful is attaching an MFA to the log lines. For example:

```erlang
{install, {fun tracer/3,
    #{
        metadata => #{mfa => {?MODULE, handle_event, 4}}
    }
}}
```

Then later:

```erlang
tracer(TraceState = #{metadata := Metadata}, Event, StateData) ->
    logger:log(debug, "~tp", [Event], Metadata),
    TraceState.
```

## FuncId

One thing that you need to be careful of is that `install` comes in two forms:
- `{install, {Func, FuncState}}`
- `{install, {FuncId, {Func, FuncState}}}`

If you pass a 2-tuple (or a single-item record) as `FuncState`, like this...

```erlang
{install, {fun tracer/3, Tuple2}}
```

...then it gets misinterpreted as the latter -- the `fun` gets passed as the ID, and the first element of the tuple (or the record tag) gets used as the `Func`. So it doesn't work.

To avoid this, always explicitly use the second form (as shown at the top of the page):

```erlang
{install, {?MODULE, {fun tracer/3, TraceState}}}
```

## Performance

`gen_statem` and `gen_server` go out of their way to optimise for the case where `debug` is unset, so you might want to think about how to enable and disable this, _before_ it gets as far as `logger`. Something like this would work:

```erlang
start_link() ->
    gen_statem:start_link(?MODULE, [], start_options()).

start_options() ->
    case application:get_env(?APPLICATION, enable_trace, false) of
        false -> [];
        true -> [{debug, [{install, ...}]}]
    end.
```

## Formatting the messages

One nice thing that `gen_statem` does is to format the events nicely; see [`gen_statem:print_event/3`](https://github.com/erlang/otp/blob/OTP-27.0/lib/stdlib/src/gen_statem.erl#L2867). You'll want to replicate that functionality in your tracer.

