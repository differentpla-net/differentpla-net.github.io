---
title: "Phoenix, without"
date: 2025-08-25T14:06
tags: elixir-phoenix
---

Installing phoenix

```
mix archive.install hex phx_new
```

When you use `mix phx.new hello`, it generates a Phoenix application with a bunch of stuff that you might not want.

You get:
- Ecto, for databases.
- Phoenix.HTML, TailwindCSS, Esbuild
- Phoenix.LiveView

If you just want an API, you can use `mix phx.new my_api --no-html --no-assets`; this includes Ecto. If you want bare-bones, you can use:

```sh
mix phx.new nada --no-assets --no-dashboard --no-ecto --no-esbuild --no-gettext --no-html --no-live --no-mailer --no-tailwind
```

Even then, it includes other crap, like telemetry and dns_cluster. Also enables Phoenix.PubSub. Even though I specified
no live view, it still put some config in there for it.

Add an interlude here where I had to recompile elixir, and refetch everything.

And you don't have a route for /, which is fair, I guess. MIX_ENV=prod results in a JSON error, rather than a stack
trace. `config/dev.exs` has `debug_errors: true`

At that point, though, you might as well use Bandit and Plug by themselves, I guess.

What's in there?

We start `Nada.Endpoint` as a supervisor child. That sets up a bunch of plugs. How does that even work? Presumably the
`use Phoenix.Endpoint` interacts with `plug` to compose the endpoint. The last plug in the list is `NadaWeb.Router`.

Yeah; it eventually does `use Plug.Builder`, which defines the `plug` macro, which adds a `@plugs` module attribute,
which is then traversed to build the plug chain. Magic.

That does `use NadaWeb, :router`. I've always thought that was kinda inside-out. What it _does_ is effectively import
the content of `NadaWeb.router` into the module. Why you'd want to do that instead of just putting it in there as
boilerplate, I dunno. I guess for controllers and channels, it means you can have the same boilerplate in each one. Less
important for routers, 'cos you'd usually only have one of them.

It then defines a pipeline and a scope.

So: if you're looking for an interactive web app, you're gonna be writing a bunch of JS/API code, or you're gonna be
using LiveView (or similar, in other ecosystems).
