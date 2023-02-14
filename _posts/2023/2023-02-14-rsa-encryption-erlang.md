---
title: "Using RSA keys for encryption and decryption in Erlang"
date: 2023-02-14T09:39:00Z
tags: erlang cryptography
---

How do I encrypt (and decrypt) things in Erlang, using an RSA key?

It's important to note that most encryption schemes use symmetric encryption, and only use asymmetric encryption for key
exchange.

The way this works is that you encrypt using the recipient's public key, and the recipient decrypts using their private
key.

## Generating the key pair

We'll generate a private key:

```erlang
Size = 2048,  % in bits
Exp = 65537,  % standard RSA exponent
RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}).
```

Then we derive the public key. Senders will encrypt their messages with this, so that only we can decrypt it.

```erlang
rr(public_key).   % in the REPL

#'RSAPrivateKey'{publicExponent = E, modulus = N} = RSAPrivateKey.
RSAPublicKey = #'RSAPublicKey'{modulus = N, publicExponent = E}.
```

## Encrypting a message

Note that we use the public key.

```erlang
PlainText = <<"Hello World">>.
CipherText = public_key:encrypt_public(PlainText, RSAPublicKey).
```

Passing no options to `public_key:encrypt_public/2` uses the default `[{rsa_padding, rsa_pkcs1_padding}]`.

<div class="callout callout-warning" markdown="span">
The padding options used for encryption and decryption must match.
</div>

## Decrypting a message

To decrypt, we use the private key.

```erlang
PlainText = public_key:decrypt_private(CipherText, RSAPrivateKey).  % <<"Hello World">>
```
