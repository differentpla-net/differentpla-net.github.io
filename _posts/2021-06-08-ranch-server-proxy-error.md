---
title: "failed to start child: :ranch_server_proxy"
date: 2021-06-08T14:02:00Z
tags: elixir ranch
---

```
** (Mix) Could not start application example_server: ExampleServer.Application.start(:normal, []) returned an error: shutdown: failed to start child: {:ranch_embedded_sup, :example_server}
    ** (EXIT) shutdown: failed to start child: :ranch_server_proxy
        ** (EXIT) no process: the process is not alive or there's no process currently associated with the given name, possibly because its application isn't started
```

Did you start the `ranch` application? If you're using `rebar3` (Erlang) or `mix` (Elixir), it ought to start automatically as part of your application.

Unless you've marked it as `optional: true` (which is what I did -- I'd copy-pasted it from another project).

Instead of:

```elixir
  defp deps do
    [
      {:ranch, "~> 2.0", optional: true}
      # don't do this ---^
    ]
  end
```

You want this:

```elixir
  defp deps do
    [
      {:ranch, "~> 2.0"}
    ]
  end
```

Facepalm.
