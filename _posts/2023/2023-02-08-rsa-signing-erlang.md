---
title: "Wrapping an RSA private key with Erlang"
date: 2023-02-08T14:06:00Z
tags: erlang cryptography
---

How do I sign (and verify) things in Erlang, using an RSA key?

We'll start with a private key:

```erlang
Size = 2048,  % in bits
Exp = 65537,  % standard RSA exponent
RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}).
```

If you want to check this against openssl, write the key to a file; see [this post]({% post_url 2023/2023-02-08-wrap-rsa-private-key-erlang %}).

## Signing a message

```erlang
Signature = public_key:sign(<<"Hello World">>, sha256, RSAPrivateKey).
```

You can also use `crypto:sign/4`, but you'll need to decompose the `RSAPrivateKey` record first:

```erlang
rr(public_key).   % in the REPL

#'RSAPrivateKey'{publicExponent = E, modulus = N, privateExponent = D} = RSAPrivateKey.
Signature = crypto:sign(rsa, sha256, <<"Hello World">>, [E, N, D]).
```

Or, using the longer key format (which is faster):

```erlang
rr(public_key).   % in the REPL

#'RSAPrivateKey'{publicExponent = E, modulus = N, privateExponent = D, prime1 = P1, prime2 = P2, exponent1 = E1, exponent2 = E2, coefficient = C} = RSAPrivateKey.
Signature = crypto:sign(rsa, sha256, <<"Hello World">>, [E, N, D, P1, P2, E1, E2, C]).
```

All of the above should return the same value for `Signature`.

You can compare it with `openssl`, as follows:

```sh
echo -n 'Hello World' | openssl dgst -sha256 -sign my.key | base64
```

## Verifying a message

To verify a message, you need only the public key:

```erlang
#'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = RSAPrivateKey.
RSAPublicKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent}.
```

```erlang
true = public_key:verify(<<"Hello World">>, sha256, Signature, RSAPublicKey).
```

But it also works with the private key:

```erlang
true = public_key:verify(<<"Hello World">>, sha256, Signature, RSAPrivateKey).
```

If you want to use `crypto:verify/5`, that looks like this:

```erlang
true = crypto:verify(rsa, sha256, <<"Hello World">>, Signature, [E, N]).
```

You can compare it with openssl, but note that you'll need the public key (`my.pub`) and the signature should be in a file (here, `sig`):

```
$ echo -n 'Hello World' | openssl dgst -sha256 -verify my.pub -signature sig
Verified OK
```

If it fails:

```
$ echo -n 'Hello World!' | openssl dgst -sha256 -verify my.pub -signature sig
Verification Failure
```
