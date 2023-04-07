---
title: "Inspecting certificates in Erlang"
date: 2023-04-07T13:35:00.000Z
tags: erlang certificates
---

After successfully rotating the CA certificates in my Erlang cluster (see the earlier post), I wanted to verify that the
nodes were _actually_ using certificates from different CA certificates. Here's how to do that in Erlang.

## Parse the TLS configuration file

I already know which certificate file to inspect, but for completeness, here's how to figure out what Erlang's TLS
distribution is using:

```erlang
{ok, [[OptFile]]} = init:get_argument(ssl_dist_optfile).
{ok, [Opts | _]} = file:consult(OptFile).
CertFile = proplists:get_value(certfile, proplists:get_value(server, Opts)).
```

## Load the certificate

```erlang
{ok, Pem} = file:read_file(CertFile).
[CertEntry] = public_key:pem_decode(Pem).
Cert = public_key:pem_entry_decode(CertEntry).
```

TODO: This doesn't work in erlclu, 'cos I stripped debug info / don't have source code: `rr(public_key)` fails.

```erlang
[{_,Der,_}] = public_key:pem_decode(Pem).
OTPCert = public_key:pkix_decode_cert(Der, otp).
```

```erlang
public_key:pkix_issuer_id(OTPCert, self).
```

```erlang
CACertFile = proplists:get_value(cacertfile, proplists:get_value(server, Opts)).
{ok, CAPem} = file:read_file(CACertFile).
CACerts = [public_key:pkix_decode_cert(Der, otp) || {_, Der, _} <- public_key:pem_decode(CAPem)].
```

```erlang
lists:filter()
