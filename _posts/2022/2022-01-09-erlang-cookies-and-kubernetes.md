---
title: "Erlang/Elixir Cookies and Kubernetes"
date: 2022-01-09T10:27:00Z
tags: erlang elixir kubernetes
---

Distributed Erlang and Elixir applications use a shared secret called a "cookie".  It's just a string of alphanumeric
characters. All nodes in the cluster must use the same cookie. Let's take a look at what that means in a Kubernetes
context.

**tl;dr:** Use Kubernetes secrets to set the `RELX_COOKIE` (Erlang) or `RELEASE_COOKIE` (Elixir) environment variable.

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
$ erl -sname foo -cookie KMZWIWWTBVPEBURCLHVQ
(foo@rpi401)1> erlang:get_cookie().
'KMZWIWWTBVPEBURCLHVQ'

$ iex --sname foo --cookie KMZWIWWTBVPEBURCLHVQ
iex(foo@rpi401)1> Node.get_cookie()
:KMZWIWWTBVPEBURCLHVQ
```

<div class="callout callout-warning" markdown="span">
By today's standards, Erlang cookies would not be considered good security. They're relatively-easily guessable
and sniffable. If you want a properly-secure cluster, you need to use TLS with mutual authentication, and you should set
up your network policy to restrict which pods can communicate with each other.
</div>

## Erlang releases

If you're using [`relx`](https://github.com/erlware/relx) to build your Erlang releases, you're probably specifying the
cookie in `vm.args`; here's the relevant snippet from the template:

```conf
## Cookie for distributed erlang
{% raw %}-setcookie {{ rel_name }}{% endraw %}
```

Yes, it's predictable; you can change it. The point is that, because `vm.args` is part of the release, every node will
be using the same cookie, and you're done.

Unfortunately, `vm.args` is usually checked into source control. You should prefer setting it with the `RELX_COOKIE`
environment variable.

<div class="callout callout-warning" markdown="span">
Rotating cookies is _hard_. I'm not going to cover it here.
</div>

## Elixir releases

If you're using [`mix release`](https://hexdocs.pm/mix/Mix.Tasks.Release.html), a random cookie is created at build time
and written to `_build/prod/rel/myapp/releases/COOKIE`. This means that the cookie changes every time you build from
clean, which will break your cluster. Alternatively, you can use a ConfigMap

You can specify a cookie with the `:cookie` option in `mix.exs`. I would avoid doing this because it means that the
cookie is now visible in source control history.

The other way to set the cookie is to set the `RELEASE_COOKIE` environment variable before starting the release. You can
do this in `rel/env.sh.eex`, or from a Kubernetes secret.

## Secrets

```bash
ERLANG_COOKIE=$(head -c 40 < /dev/random | base64 | tr '/+' '_-')
kubectl --namespace myapp create secret generic erlang-cookie --from-literal=cookie="$ERLANG_COOKIE"
```

Remember:
- Secrets are scoped to the namespace, so you might want to put your app name as a prefix, unless you're using a dedicated namespace.
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
  - name: RELEASE_COOKIE  # or RELX_COOKIE (or just set both)
    valueFrom:
      secretKeyRef:
        name: erlang-cookie
        key: cookie
```

## Why not use ConfigMaps?

Because they're basically the same as Secrets.

## Cookie generation algorithms (WIP)

Elixir:

```elixir
iex(1)> Base.url_encode64(:crypto.strong_rand_bytes(40))
"FW9RYfQpVbuycMxSodrXIKAzuLgsaR5gyArGeap8WTHNfJj3vfltYQ=="
```
