---
title: "Erlang Shell, etc."
date: 2023-03-04T15:59:00Z
tags: erlang
---

A series about the Erlang shell, etc.

## Outline

- I/O protocol.
- Group Leader.
- Shell, compared with Elixir.
- user_default, shell_default, etc.

## Other resources

- https://azunyanmoe.wordpress.com/2011/04/05/the-erlangrc-file/ -- loading non-default .erlang
- https://github.com/kennethlakin/erlangrc -- loading user_default


## Shell

When you start `erl` (or `iex`), the BEAM runtime starts an interactive shell. For Erlang, that's `shell:start()`; for Elixir ... ???

It's started from ...???

Look for `-noshell`, 'cos that'll be the other branch of the if statement.

It's done in `user_drv:start/0`, which is called from `shell:start_interactive`.

Elixir starts the shell like this:

```elixir
defmodule IEx.CLI do
  def start do
    if tty_works?() do
      :user_drv.start([:"tty_sl -c -e", tty_args()])
```

This, interestingly, has a backwards-compat hack in `user_drv`:

```erlang
%% Backwards compatibility with pre OTP-26 for Elixir/LFE etc
start(['tty_sl -c -e', Shell]) ->
    start(#{ initial_shell => Shell });
```

In the case of Elixir, tty_args resolves to this (I'm ignoring remote shells):

```elixir
  defp local_start_mfa do
    {IEx, :start, [options(), {:elixir, :start_cli, []}]}
  end
```

There'll be something in the job control (Ctrl+G) that starts shells as well, since you can start a custom shell from there, too. That is: in Elixir, Ctrl+G, s will start the Erlang shell by default.
