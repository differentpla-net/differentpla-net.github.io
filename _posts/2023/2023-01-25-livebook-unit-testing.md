---
title: "Unit testing in Elixir Livebook"
date: 2023-01-25T11:19:00Z
tags: livebook testing
---

I've written some code in Elixir Livebook, and I'd like to write some unit tests for it. Here's how I did it.

Add a code block with the following:

```elixir
ExUnit.configure(exclude: [:skip])
ExUnit.start(autorun: false)
```

The `exclude: [:skip]` part allows you to mark tests with `@tag :skip` to, well, skip them. There's an example of this
below.

Then, for each set of tests:

```elixir
defmodule FooTest do
  use ExUnit.Case

  test "it works" do
    assert true
  end
end

ExUnit.run()
```

```elixir
defmodule BarTest do
  use ExUnit.Case

  test "it doesn't not work" do
    refute false
  end

  @tag :skip
  test "this is skipped" do
    assert :up == :down
  end
end

ExUnit.run()
```
