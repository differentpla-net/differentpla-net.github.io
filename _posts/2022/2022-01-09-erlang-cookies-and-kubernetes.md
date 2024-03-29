---
title: "Erlang/Elixir Cookies and Kubernetes"
date: 2022-01-09T10:27:00Z
tags: erlang elixir kubernetes
---

Distributed Erlang and Elixir applications use a shared secret called a "cookie".  It's just a string of alphanumeric
characters. All nodes in the cluster must use the same cookie. Let's take a look at what that means in a Kubernetes
context.

**tl;dr:** Use Kubernetes secrets to set the `RELEASE_COOKIE` environment variable. For Erlang, also edit
`config/vm.args.src`.

## The `~/.erlang.cookie` file

By default, Erlang reads the cookie from `~/.erlang.cookie`:

```
$ cat ~/.erlang.cookie
MQATXXBUYSOQAYUNJPWZ

$ erl -sname foo
(foo@rpi401)1> erlang:get_cookie().
'MQATXXBUYSOQAYUNJPWZ'

$ iex --sname foo
iex(foo@rpi401)1> Node.get_cookie()
:MQATXXBUYSOQAYUNJPWZ
```

Note that you need to enable distribution by using `-sname` or `-name`. Otherwise `erlang:get_cookie()` returns `nocookie`.

If the `~/.erlang.cookie` file doesn't exist, it will be created and a randomly-generated string will be written to it.

Alternatively, you can specify the cookie on the command-line:

```
$ erl -sname foo -setcookie KMZWIWWTBVPEBURCLHVQ
(foo@rpi401)1> erlang:get_cookie().
'KMZWIWWTBVPEBURCLHVQ'

$ iex --sname foo --cookie KMZWIWWTBVPEBURCLHVQ
iex(foo@rpi401)1> Node.get_cookie()
:KMZWIWWTBVPEBURCLHVQ
```

<div class="callout callout-warning" markdown="span">
By today's standards, Erlang cookies would *not* be considered good security. They're relatively-easily guessable
and sniffable. If you want a properly-secure cluster, you need to use TLS with mutual authentication, and you should set
up your network policy to restrict which pods can communicate with each other.
</div>

## Erlang releases

If you're using [`relx`](https://github.com/erlware/relx) to build your Erlang releases, you're probably specifying the
cookie in `vm.args`; here's the relevant snippet from the template:

```conf
## Cookie for distributed erlang
{% raw %}-setcookie {{name}}_cookie{% endraw %}
```

...which, when rebar creates your app, expands to (e.g.):

```conf
## Cookie for distributed erlang
-setcookie myapp_cookie
```

Because `vm.args` is part of the release, every node will be using the same cookie, which is what you want. Yes, it's
predictable; you can change it.

<div class="callout callout-warning" markdown="span">
Rotating cookies is _hard_. I'm not going to cover it here.
</div>

Unfortunately, `vm.args` is usually checked into source control, which means that your cookie is checked into source
control.

Instead, rename `vm.args` to `vm.args.src`, and change it as follows:

```conf
## Cookie for distributed erlang
{% raw %}-setcookie ${RELEASE_COOKIE}{% endraw %}
```

You'll also have to update your `relx.config` (or the `relx` section in `rebar.config`) to include the following:

```erlang
{vm_args_src, "./config/vm.args.src"}
```

<div class="callout callout-info" markdown="span">
This is not needed since rebar 3.14.0. It automatically uses the presence of `config/vm.args.src` to enable this mode.
</div>

If you do the above, then the startup script will automatically expand environment variables at runtime, meaning that
you can set control the cookie by setting the `RELEASE_COOKIE` environment variable.

## Elixir releases

If you're using [`mix release`](https://hexdocs.pm/mix/Mix.Tasks.Release.html), a random cookie is created at build time
and written to `_build/prod/rel/myapp/releases/COOKIE`. This means that the cookie changes every time you build from
clean, which will break your cluster.

You can specify a cookie with the `:cookie` option in `mix.exs`. I would avoid doing this because it means that the
cookie is now visible in source control history.

The other way to set the cookie is to set the `RELEASE_COOKIE` environment variable before starting the release. You can
do this in `rel/env.sh.eex`, or from a Kubernetes secret.

## Secrets

```bash
ERLANG_COOKIE=$(head -c 40 < /dev/random | base64 | tr '/+' '_-')
kubectl --namespace myapp \
    create secret generic erlang-cookie \
        --from-literal=cookie="$ERLANG_COOKIE"
```

Remember:
- Secrets are scoped to the namespace, so you might want to put your app name as a prefix, unless you're using a
  dedicated namespace.
- A secret can contain multiple items. The example above uses `cookie` as the key.
- Secrets aren't actually that secret. Fortunately, Erlang cookies aren't actually that secret either.

There don't appear to be any widespread conventions for this yet. RabbitMQ recommends a dedicated namespace and uses
`erlang-cookie` as the secret name and `cookie` as the key; see [this blog post](https://blog.rabbitmq.com/posts/2020/08/deploying-rabbitmq-to-kubernetes-whats-involved/).

## Environment variables

```yaml
# kind: Pod, StatefulSet, ReplicaSet or Deployment
containers:
- name: myapp
  env:
  - name: RELEASE_COOKIE
    valueFrom:
      secretKeyRef:
        name: erlang-cookie
        key: cookie
```

## Why not use ConfigMaps?

Because they're basically the same as Secrets.

## Cookie generation algorithms

The code to generate a new Erlang cookie is in `lib/kernel/src/auth.erl`. It generates 20 random characters from `[A-Z]`.

It doesn't actually use the standard RNG; it uses one taken from _Knuth: The Art of Computer Programming, Volume II, Seminumerical Algorithms_. I can only assume that it does this because the rest of the runtime (including the standard RNG) isn't available yet.

It's broadly the equivalent of this:

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
```

Elixir does this:

```elixir
iex(1)> Base.url_encode64(:crypto.strong_rand_bytes(40))
"FW9RYfQpVbuycMxSodrXIKAzuLgsaR5gyArGeap8WTHNfJj3vfltYQ=="
```
