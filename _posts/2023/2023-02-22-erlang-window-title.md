---
title: "Setting the window title from Erlang"
date: 2023-02-22T17:21:00Z
tags: erlang
---

How do I change the title of my terminal window from Erlang?

Maybe your shell updates the window title to the currently-running command? If all of your windows say "erl" or "make
shell" or "rebar3 shell", it's going to be hard to tell them apart. Here's how to set the window title from Erlang,
assuming an xterm-compatible terminal emulator.

```erlang
set_window_title(Title) when is_list(Title) ->
    Seq = "\e]0;" ++ Title ++ "\007",
    open_port({spawn, "echo -ne \"" ++ Seq ++ "\""}, [nouse_stdio]),
    ok.
```

The trick with `open_port` is required; if you try to use `io:format`, the Erlang shell hangs. I think I found it on the
Erlang mailing list.

## Other stuff

- https://www.proctor-it.com/erlang-thursday-user_default/
- https://www.erlang.org/doc/man/erl.html#configuration
- https://www.erlang.org/doc/man/shell.html#prompt_func-1
- https://www.erlang.org/doc/man/shell_default.html
- https://azunyanmoe.wordpress.com/2011/04/05/the-erlangrc-file/
