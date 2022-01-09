---
title: "Colouring the 'iex' prompt"
date: 2022-01-09T12:17:00Z
tags: elixir
---

If you've got separate dev, test and prod environments, it can help to use colour to tell you which one you're logged
into. Here's how to colour the `iex` prompt appropriately.

Create `.iex.exs` containing the following:

```elixir
Application.put_env(:elixir, :ansi_enabled, true)

get_colour = fn ->
  case System.get_env("WHICH_ENVIRONMENT") do
    nil -> :white
    "dev" -> :green
    "test" -> :yellow
    _ -> :red
  end
end

default_prompt = ["%prefix(%counter)>"]
alive_prompt = ["%prefix(", get_colour.(), "%node", :reset, ")%counter>"]

IEx.configure(
  colors: [enabled: true],
  default_prompt: default_prompt |> IO.ANSI.format() |> IO.chardata_to_string(),
  alive_prompt: alive_prompt |> IO.ANSI.format() |> IO.chardata_to_string()
)
```

This applies the colour to only the node name, so it only works with `--sname` or `--name` (or in a remote console).
Feel free to edit it for your needs.

The name of the environment variable (`WHICH_ENVIRONMENT`), and the possible values for it (`dev`, `test`, etc.), are
completely up to you (or your ops team; go and talk to them). I've assumed that the variable will be unset on your local
machine, and that unrecognised values should be treated as if they were `prod` (because potentially dangerous).

Elixir searches for this file in the current directory, then in `$HOME`, so you should just put it at the top level of
your project. If you're using containers, copy it to `$HOME` with something like this:

```dockerfile
WORKDIR /home/app
# ... other stuff
COPY .iex.exs .
```
