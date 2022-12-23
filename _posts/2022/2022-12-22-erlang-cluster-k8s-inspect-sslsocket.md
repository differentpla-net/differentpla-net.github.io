---
title: "Erlang cluster on Kubernetes: Is it mutual?"
short_title: "Is it mutual?"
date: 2022-12-22T16:45:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang
---

In theory, we've [got TLS working]({% post_url 2022/2022-12-22-erlang-cluster-k8s-tls-distribution %}) for our Erlang
cluster, with [mutual authentication]({% post_url 2022/2022-12-22-erlang-cluster-k8s-use-certs %}). How do we prove
that?

Here's one way. It's a complete hack. It relies on internal details of the Erlang runtime. Don't try this at home.

## Listing connections

If you run `inet:i().` in the Erlang console, you'll see a table of all of the active TCP sockets in the process. It
looks like this:

```
(erlclu@10.42.2.46)1> inet:i().
Port Module   Recv  Sent    Owner     Local Address    Foreign Address  State        Type
24   inet_tcp 0     0       <0.489.0> *:46025          *:*              ACCEPTING    STREAM
40   inet_tcp 6     21      <0.487.0> localhost:40110  localhost:epmd   CONNECTED(O) STREAM
64   inet_tcp 0     0       <0.570.0> *:10022          *:*              ACCEPTING    STREAM
72   inet_tcp 0     0       <0.573.0> *:http-alt       *:*              ACCEPTING    STREAM
80   inet_tcp 0     0       <0.586.0> *:9153           *:*              ACCEPTING    STREAM
216  inet_tcp 4591  4914    <0.663.0> 10.42.2.46:47606 10.42.1.94:46881 CONNECTED(O) STREAM
232  inet_tcp 4589  4915    <0.675.0> 10.42.2.46:39022 10.42.0.88:35897 CONNECTED(O) STREAM
296  inet_tcp 4609  4358    <0.693.0> 10.42.2.46:46025 10.42.2.47:33860 CONNECTED(O) STREAM
320  inet_tcp 3354  3793    <0.701.0> 10.42.2.46:46025 10.42.0.89:42484 CONNECTED(O) STREAM
368  inet_tcp 3247  1524923 <0.710.0> 10.42.2.46:9153  10.42.0.15:35612 CONNECTED(O) STREAM
648  inet_tcp 19472 73069   <0.740.0> localhost:10022  localhost:36766  CONNECTED(O) STREAM
ok
```

There are a number of different connections in there. We can identify them as follows:

- The ones on port `10022` are the [SSH daemon]({% post_url 2022/2022-12-22-erlang-cluster-k8s-ssh %}) and the current
  remote console connection.
- The one listening on `http-alt` is the [cowboy endpoint]({% post_url 2022/2022-12-21-erlang-cluster-k8s-http-service %}).
- The one connected to `epmd` is, well, connected to epmd.
- The one listening on `9153` is the metrics endpoint (the subject of a future blog post).
- This means that the one listening on port `46025` is probably the TLS distribution endpoint.
- The others have port `46025` in the local address (TLS server) or in the remote address (TLS client).

We can confirm the distribution port by asking epmd:

```
(erlclu@10.42.2.46)2> erl_epmd:names().
{ok,[{"erlclu",46025}]}
```

Yep. Port 46025 it is.

So all we need to do is -- somehow -- convert the owner into something that we can pass to
`ssl:connection_information/1`, and we'll be able to ask questions about that connection, including whether it's using
mutual authentication.

## The hack

Here's how to turn that owner pid into an `#sslsocket{}` record, suitable for passing to `ssl:connection_information`.

```erlang
-module(erlclu_inspect).
-export([sslsocket_from_pid/1]).

% You've got a TLS socket in inet:i(), and you want to use
% ssl:connection_information with it.
% DO NOT DO THIS!
sslsocket_from_pid(Pid) ->
    {connection, State} = sys:get_state(Pid),
    StaticEnv = element(2, State),
    Port = element(11, StaticEnv),
    ProtocolSpecific = element(9, State),
    Pid2 = maps:get(sender, ProtocolSpecific),

    Transport = gen_tcp,
    ConnectionCb = tls_connection,
    {sslsocket, {Transport, Port, ConnectionCb, undefined}, [Pid, Pid2]}.
```

<div class="callout callout-warning" markdown="span">
It's tightly-coupled to the implementation details. I've only tested it on Erlang/OTP-25.1.2. Basically: don't do this.
</div>

## Client authentication

We assume that server verification works. We're interested in whether we're using client certificates for
authentication.

We need to look at the connections which have us as the server. Specifically, these two:

```
296  inet_tcp 4609  4358    <0.693.0> 10.42.2.46:46025 10.42.2.47:33860 CONNECTED(O) STREAM
320  inet_tcp 3354  3793    <0.701.0> 10.42.2.46:46025 10.42.0.89:42484 CONNECTED(O) STREAM
```

By converting one of those into an sslsocket, we can get the peer cert:

```
(erlclu@10.42.2.46)3> S = erlclu_inspect:sslsocket_from_pid(pid(0,701,0)).
{sslsocket,{gen_tcp,#Port<0.40>,tls_connection,undefined},
           [<0.701.0>,<0.700.0>]}
(erlclu@10.42.2.46)4> ssl:peercert(S).
{ok,<<48,130,1,183,48,130,1,94,160,3,2,1,2,2,16,79,236,
      181,165,226,176,210,127,102,251,166,71,...>>}
```

It _has_ a certificate. That means it's using mutual TLS. Job done.
