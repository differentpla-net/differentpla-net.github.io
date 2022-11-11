---
title: "Erlang clustering recap"
date: 2022-11-11T19:24:00Z
tags: erlang
---

I want to write a post about using mutual TLS to secure Erlang distribution (clustering), with auto-provisioning of
certificates when running in Kubernetes. This is not that post. This is a recap of basic Erlang clustering, to refresh
my memory and lay some groundwork.

To cluster Erlang nodes, they must be started with `-name` or `-sname`; they must be able to resolve each other with
that name, and they must be using the same cookie.

Let's start two Erlang nodes. To start with, we'll start them on the same node:

```
$ erl -sname foo
(foo@ROGER-SURFACE-GO)1>
```

```
$ erl -sname bar
(bar@ROGER-SURFACE-GO)1>
```

The annoying upper-case name is because I'm using WSL; I'll live with it.

Because I used `-sname`, Erlang used the short machine name: `ROGER-SURFACE-GO` (as returned from `hostname`).

If I'd used `-name`, it would have used `ROGER-SURFACE-GO.localdomain` (as returned from `hostname -f`). The
`.localdomain` is a WSL thing, again.

We can find out which nodes are connected to a node by using the `nodes/0` function:

```
(foo@ROGER-SURFACE-GO)1> nodes().
[]
```

No nodes. We can connect to the other node with the following:

```
(foo@ROGER-SURFACE-GO)2> true = net_kernel:connect_node('bar@ROGER-SURFACE-GO').
true
(foo@ROGER-SURFACE-GO)3> nodes().
['bar@ROGER-SURFACE-GO']
```

If we check on the other node:

```
(bar@ROGER-SURFACE-GO)1> nodes().
['foo@ROGER-SURFACE-GO']
```

We can send messages to processes on another node, but we need some way to find the process. The easiest way to do that
is to use `register/2`. We'll register the shell process on the first node with a name:

```
(foo@ROGER-SURFACE-GO)4> register(shell, self()).
true
```

Then on the second node, we can send a message to the process on the first node:

```
(bar@ROGER-SURFACE-GO)1> {shell, 'foo@ROGER-SURFACE-GO'} ! hello.
```

Back on the first node, we can see that the message arrives:

```
(foo@ROGER-SURFACE-GO)5> flush().
Shell got hello
ok
```

Erlang clustering's working between two nodes running on the same host. That's a good start.

### Autoclustering using epmd

We had to explicitly name the node name in `net_kernel:connect_node`, above. Can we do that automatically?

Yes, we can. Since the two nodes are running on the same node, they'll have registered in `epmd`, the Erlang Port
Mapping Daemon. We can use that for discovery.

If you're writing an Elixir program, you can use `libcluster` for this, using its `epmd` strategy.

Since I'm writing Erlang, I'll have to do it explicitly:

```erlang
{ok, Names} = erl_epmd:names().
{ok, Host} = inet:gethostname().
[net_kernel:connect_node(list_to_atom(N ++ "@" ++ Host)) || {N, _} <- Names].
```

## Different hosts

What happens if we try to cluster nodes that are running on different hosts?

```
roger-nuc1$ erl -sname demo
(demo@roger-nuc1)1>
```

```
roger-nuc2$ erl -sname demo
(demo@roger-nuc2)1>
```

The node names (`demo`) can be the same because the nodes are running on different nodes.

We can't use epmd to discover nodes on other hosts, unless we already know the host name, so we'll have to go back to
doing it explicitly.

Can we connect them?

```
(demo@roger-nuc1)1> net_kernel:connect_node('demo@roger-nuc2').
false
```

The other host prints an error message:

```
(demo@roger-nuc2)1> =ERROR REPORT==== 11-Nov-2022::20:16:34.138961 ===
** Connection attempt from node 'demo@roger-nuc1' rejected. Invalid challenge reply. **
```

## Cookies

It's because the two nodes are using different cookies. We can fix that. Kill both of the Erlang nodes and restart them:

```
roger-nuc1$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ
(demo@roger-nuc1)1>
```

```
roger-nuc2$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ
(demo@roger-nuc2)1>
```

The cookie is an arbitrary string. The one above was randomly generated with the following:

```
env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20
```

Can we connect them?

```
(demo@roger-nuc1)1> net_kernel:connect_node('demo@roger-nuc2').
true
(demo@roger-nuc1)2> net_kernel:connect_node('demo@roger-nuc2').
['demo@roger-nuc2']
```

That works. We can repeat all of the message-sending stuff, and that works too.

Cool, so clustering's working. Next, we'll [secure it with TLS]({% post_url 2022/2022-11-12-erlang-tls-distribution %}).
