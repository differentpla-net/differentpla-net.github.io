---
title: "ALPN in Erlang"
date: 2021-03-01T11:16:00Z
tags: erlang
---

Per [Wikipedia](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation):

> Application-Layer Protocol Negotiation (ALPN) is a Transport Layer Security (TLS)
> extension that allows the application layer to negotiate which protocol should be
> performed over a secure connection in a manner that avoids additional round trips
> and which is independent of the application-layer protocols.

In Erlang, the client advertises which protocols it understands by using the `alpn_advertised_protocols` option:

```erlang
{ok, Socket} = ssl:connect("localhost", 5555,
    [{alpn_advertised_protocols,
        [<<"my-protocol/2">>, <<"my-protocol/1">>]},
     % ...
    ]).

{ok, Protocol} = ssl:negotiated_protocol(Socket).
```

The server declares which protocols it prefers by using the `alpn_preferred_protocols` option, which are specified from most-preferred to least-preferred.

Protocol names must be specified as binaries. The list of protocol names is managed by IANA.

## Of note

If the server and client can't agree on a protocol, the server closes the connection with a TLS alert.

If the client or server uses ALPN, and the other doesn't, then the connection succeeds, but `ssl:negotiated_protocol/1` returns `{error, protocol_not_negotiated}`.
