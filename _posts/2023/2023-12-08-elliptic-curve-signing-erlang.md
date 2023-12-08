---
title: "Using Elliptic Curve keys for signing in Erlang"
date: 2023-12-08T14:10:00Z
tags: erlang cryptography
---

How do I sign (and verify) things in Erlang, using an ECDSA key?

We'll start with a private key on the NIST P-256 (secp256r1, prime256v1) curve:

```erlang
PrivateKey = public_key:generate_key({namedCurve, secp256r1}).
```

## Signing a message

```erlang
Signature = public_key:sign(<<"Hello World">>, sha256, PrivateKey).
```

## Verifying a message

To verify a message, you need only the public key:

```erlang
#'ECPrivateKey'{publicKey = Pub, parameters = Params} = PrivateKey.
PublicKey = {#'ECPoint'{point = Pub}, Params}.
```

```erlang
true = public_key:verify(<<"Hello World">>, sha256, Signature, PublicKey).
```

But it also works with the private key:

```erlang
true = public_key:verify(<<"Hello World">>, sha256, Signature, PrivateKey).
```
