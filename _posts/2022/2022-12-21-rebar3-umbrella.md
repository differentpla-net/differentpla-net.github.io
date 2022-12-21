---
title: "Rebar3 Umbrella Projects"
date: 2022-12-21T14:29:00.000Z
tags: erlang
---

Some notes about using [rebar3](https://rebar3.org/) with an umbrella project.

```bash
rebar3 new umbrella name=foo      # umbrella and release are aliases
cd umbrella/
```

```bash
cd apps/
rebar3 new app name=bar
rebar3 new lib name=baz
```

It doesn't set up the application dependencies (because it can't _know_ what you mean to do), so you'll need to do that
yourself. Edit `apps/foo/src/foo.app.src`:

```erlang
  %...
  {applications,
   [kernel,
    stdlib,
    bar,
    baz
   ]},
   %...
```

Or, maybe, edit `rebar.config`:

```erlang
%...
{relx, [{release, {foo, "0.1.0"},
         [foo, bar, baz,
          sasl]},
%...
```
