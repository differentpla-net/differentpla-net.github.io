---
title: "Elixir's use functionality"
date: 2020-01-02T15:54:00.000Z
tags: elixir
---

`use` is a way that you can inject code into your module. Elixir uses this for boilerplate in `GenServer`, and Phoenix uses it for metaprogramming.

```
defmodule Foo do
  use Bar
end
```

While compiling the `Foo` module, Elixir runs the `Bar.__using__` macro, which you can define like this:

```
defmodule Bar do
  defmacro __using__(_args) do
    quote do
      def baz do
        :quux
      end
    end
  end
end
```

This injects the quoted code (the `baz` function) into the `Foo` module.

You can do something with the args. For example, in Phoenix, you invoke it like this:

```
use FooWeb, :controller
```

Phoenix uses it to call another function in the module:

```
defmacro __using__(which) when is_atom(which) do
  apply(__MODULE__, which, [])
end
```

...which, in turn, returns the quoted code:

```
def controller do
  quote do
    use Phoenix.Controller, namespace: MyWeb

    import Plug.Conn
    import MyWeb.Gettext
    alias MyWeb.Router.Helpers, as: Routes
  end
end
```

This could have been done with matching on the args. For example:

```
defmacro __using__(:controller) do
  quote do
    # ...
  end
end
```

...etc.

**Important: `__using__` must be a macro.**
