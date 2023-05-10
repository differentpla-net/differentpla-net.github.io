---
title: "gen_server debug options: how it works"
date: 2023-05-10T12:59:00.000Z
tags: erlang
---

In the [previous post]({% post_url 2023/2023-05-10-erlang-gen-server-debug %}), I discussed how to use debug options
with Erlang's `gen_server`. But how does it work?

## Calling `handle_debug`

When a `gen_server` (or `gen_statem`, etc.) wants to emit a debug event, it calls `sys:handle_debug`. Here's an example from the `gen_server` source code (lightly reformatted and heavily ellided):

```erlang
decode_msg(Msg, Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
        % ...snip...
	    _Msg ->
	        Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {in, Msg}),
	        handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout, Debug1)
    end.
```

Before dispatching the message, `gen_server` calls `sys:handle_debug`, passing the debug state (in `Debug`) -- this is
not the same as the debug options (more on this later), a callback function for rendering the event, the server name,
and the event -- `{in, Msg}` -- that should be reported.

## TODO

- Converting options into debug state.
- How do the sys:trace, etc., functions work -- system messages.
- Why return the new debug state? Because the `install` functions can remove themselves.
