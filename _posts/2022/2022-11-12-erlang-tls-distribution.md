---
title: "Erlang TLS Distribution"
date: 2022-11-12T14:29:00Z
tags: erlang
---

In the previous post, I [recapped Erlang distribution (clustering)]({% post_url 2022/2022-11-11-erlang-clustering-recap %}).
In this post, we'll secure it by using TLS.

By default, Erlang uses a plain-text TCP protocol for distribution, which means it's not secure. It supports using TLS.
This post will explore how to use that.

The documentation's [here](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html), but it's quite dense. I'm going
to come at it from a different angle.

Recall that in the [previous post]({% post_url 2022/2022-11-11-erlang-clustering-recap %}), we'd got two nodes, each on
a different host, talking to each other.

<div class="callout callout-info" markdown="span">
I'm using Erlang/OTP 25.0.4. This stuff will _probably_ work on Erlang/OTP 24.x.
</div>

## Specifying Distribution Module for net_kernel

This time, let's turn on TLS distribution with `-proto_dist inet_tls`:

```
roger-nuc1$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ -proto_dist inet_tls
(demo@roger-nuc1)1>
```

```
roger-nuc2$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ -proto_dist inet_tls
(demo@roger-nuc2)1>
```

This time when we attempt to connect them, well, it's not pretty:

```
(demo@roger-nuc2)2> net_kernel:connect_node('demo@roger-nuc1').
=WARNING REPORT==== 12-Nov-2022::14:38:05.798472 ===
Description: "Authenticity is not established by certificate path validation"
     Reason: "Option {verify, verify_peer} and cacertfile/cacerts is missing"
```

That's bad enough, but on the other node, I get _several pages_ of error logging:

```
(demo@roger-nuc1)1> =ERROR REPORT==== 12-Nov-2022::14:38:05.921992 ===
** State machine <0.102.0> terminating
** Last event = {internal,
                    {client_hello,
                        {3,3},
                        <<78,188,119,203,5,224,249,70,195,11,72,92,218,219,56,
                          77,170,207,16,67,176,166,188,143,91,129,82,8,119,108,
                          158,198>>,
                        <<>>,undefined,
```

...and so on.

## Specifying TLS Options

We'll need to specify some TLS options by using the `-ssl_dist_optfile` option. It takes a file name containing the
options. Let's start with this; I'm going to call it `inet_tls.conf`:

```erlang
[
  {server, [
    {certfile, "server.crt"},
    {keyfile, "server.key"},
    {secure_renegotiate, true}
  ]},
  {client, [
    {secure_renegotiate, true}
  ]}
].
```

Note that this doesn't use the correct server name, and doesn't do client authentication. Let's just get encryption
working first. We'll worry about mutual authentication later.

The documentation also says that the `certfile` must be a PEM file containing both the certificate and key. This isn't
true; you can use `certfile` and `keyfile` to specify them separately.

Note that you can pass the options on the command line using `-ssl_dist_opt`, but the documentation says that's legacy,
and I'd prefer a file anyway, because (later) when I'm doing this in Kubernetes, I can use a ConfigMap.

We're going to need a server certificate. I'll use my [elixir-certs script]({% post_url 2021/2021-12-21-elixir-certs %}):

```bash
./certs self-signed \
    --out-cert inet-tls-ca.crt --out-key inet-tls-ca.key \
    --template root-ca \
    --subject "/C=GB/L=London/O=differentpla.net/CN=differentpla.net inet_tls CA"

./certs create-cert \
    --issuer-cert inet-tls-ca.crt --issuer-key inet-tls-ca.key \
    --out-cert server.crt --out-key server.key \
    --template server \
    --subject '/CN=server'
```

We'll need the `inet_tls.conf`, `server.crt` and `server.key` files on both hosts. _You_ can use (e.g.) SSH to copy
them; _I_ just copy-pasted between two terminal windows.

```
roger-nuc1$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ -proto_dist inet_tls -ssl_dist_optfile "$PWD/inet_tls.conf"
(demo@roger-nuc1)1>
```

```
roger-nuc2$ erl -sname demo -setcookie KMZWIWWTBVPEBURCLHVQ -proto_dist inet_tls -ssl_dist_optfile "$PWD/inet_tls.conf"
(demo@roger-nuc2)1>
```

Then we can connect the two nodes:

```
(demo@roger-nuc1)1> net_kernel:connect_node('demo@roger-nuc2').
=WARNING REPORT==== 12-Nov-2022::15:28:28.249514 ===
Description: "Authenticity is not established by certificate path validation"
     Reason: "Option {verify, verify_peer} and cacertfile/cacerts is missing"

true
(demo@roger-nuc1)2> nodes().
['demo@roger-nuc2']
```

Done. It's encrypted, but it's not authenticated (that's what the `WARNING REPORT` is complaining about).

## Server authentication

We can turn on server authentication by adding `{verify, verify_peer}` and `cacertfile` options to the `client` section
of the `inet_tls.conf` file as follows:

```erlang
[
  {server, [
    {certfile, "server.crt"},
    {keyfile, "server.key"},
    {secure_renegotiate, true}
  ]},
  {client, [
    {verify, verify_peer},
    {cacertfile, "inet-tls-ca.crt"},
    {secure_renegotiate, true}
  ]}
].
```

...but now clustering doesn't work:

```
(demo@roger-nuc1)1> net_kernel:connect_node('demo@roger-nuc2').
false
```

To fix that, we'll need some server certificates that actually have the correct server name in them:

```
./certs create-cert \
    --issuer-cert inet-tls-ca.crt --issuer-key inet-tls-ca.key \
    --out-cert roger-nuc1.crt --out-key roger-nuc1.key \
    --template server \
    --subject '/CN=roger-nuc1'

./certs create-cert \
    --issuer-cert inet-tls-ca.crt --issuer-key inet-tls-ca.key \
    --out-cert roger-nuc2.crt --out-key roger-nuc2.key \
    --template server \
    --subject '/CN=roger-nuc2'
```

Ideally, you'd keep the CA key separate and secure, and you'd use certificate signing requests and all of that jazz. I
don't feel like jumping through all of those hoops for a demo.

Copy the server certificates and keys to the correct hosts.

Then you need to ensure that the node is using the correct certificate files. I'm going to do some `envsubst` stuff.

First we need to rename `inet_tls.conf` to `inet_tls.conf.template`, and edit it so that it looks like the following:

```erlang
[
  {server, [
    {certfile, "${HOSTNAME}.crt"},
    {keyfile, "${HOSTNAME}.key"},
    {secure_renegotiate, true}
  ]},
  {client, [
    {verify, verify_peer},
    {cacertfile, "inet-tls-ca.crt"},
    {secure_renegotiate, true}
  ]}
].
```

Then we need a startup script. I'm calling mine `run.sh`:

```bash
#!/bin/bash

export HOSTNAME="$(hostname -s)"

envsubst < inet_tls.conf.template > "inet_tls.$HOSTNAME.conf"

erl -sname demo \
    -setcookie KMZWIWWTBVPEBURCLHVQ \
    -proto_dist inet_tls \
    -ssl_dist_optfile "$PWD/inet_tls.$HOSTNAME.conf"
```

...and that all works:

```
$ ./run.sh
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0.4  (abort with ^G)
(demo@roger-nuc1)1> net_kernel:connect_node('demo@roger-nuc2').
true
(demo@roger-nuc1)2> nodes().
['demo@roger-nuc2']
```

### Aside: Using an Intermediate CA

If you're using an intermediate CA (this is not currently supported by my `certs` script), you'll need to put that in
the server certificate file. You don't need to include the root CA certificate.

It should come _after_ the server certificate (see RFC 4346):

```
cat roger-nuc1.crt intermediate-ca.crt > roger-nuc1.pem
```

## Client Authentication

It's still not _mutual_ authentication, though. Let's sort that out. Basically, we need to make the `client` and
`server` sections of the config file look the same:

```erlang
[
  {server, [
    {certfile, "${HOSTNAME}.crt"},
    {keyfile, "${HOSTNAME}.key"},
    {verify, verify_peer},
    {cacertfile, "inet-tls-ca.crt"},
    {secure_renegotiate, true}
  ]},
  {client, [
    {certfile, "${HOSTNAME}.crt"},
    {keyfile, "${HOSTNAME}.key"},
    {verify, verify_peer},
    {cacertfile, "inet-tls-ca.crt"},
    {secure_renegotiate, true}
  ]}
].
```

And that all works.
