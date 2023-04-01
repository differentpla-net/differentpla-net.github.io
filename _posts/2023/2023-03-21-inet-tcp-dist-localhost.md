---
title: "Restricting Erlang distribution to localhost"
date: 2023-04-01T15:04:00Z
tags: erlang
---

By default, Erlang distribution listens on all available network interfaces. Sometimes you don't want this. How can we
fix it?

## Motivation

For example:

- You might want to control which users can start a remote console session. If you restrict distribution to localhost,
  you can require a user to SSH to the host before connecting.
- You might be running two nodes on the same host (or in the same pod), and want them to connect to each other, but not
  to other Erlang nodes. As above, restricting access to localhost can help with this.
- You might have your nodes on a separate subnet, and you want them connected in a cluster, but you want to restrict
  which other nodes can join the cluster.

## Firewall rules

You could use firewall (iptables, e.g.) rules.

## Erlang options

If you run the following commands to enable distribution...

```
$ erl -sname foo
1> inet:i().
Port Module   Recv Sent Owner    Local Address   Foreign Address State        Type
24   inet_tcp 0    0    <0.61.0> *:33251         *:*             ACCEPTING    STREAM
```

...then you can see that it's listening on all available interfaces (`*`).

To control this, use the `inet_dist_use_interface` option:

```
$ erl -kernel inet_dist_use_interface "{127,0,0,1}" -sname foo
1> inet:i().
Port Module   Recv Sent Owner    Local Address   Foreign Address State        Type
24   inet_tcp 0    0    <0.61.0> localhost:43741 *:*             ACCEPTING    STREAM
```

In the above, you can see that Erlang distribution is only listening on `localhost`, meaning that only Erlang nodes on
the same host (or pod) can connect to it.
