---
title: "Generating an RSA key with Erlang"
date: 2023-02-07T18:39:00Z
tags: erlang cryptography
---

I'm currently doing something that needs RSA keys, so here are some Erlang snippets showing how to create one.

## Generating an RSA key

```erlang
Size = 2048,  % in bits
Exp = 65537,  % standard RSA exponent
RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}).
```

This is the equivalent of `openssl genrsa` (using the default settings). You can be explicit: `openssl genrsa -f4 2048`.

## Writing the private key as PEM (PKCS#1)

```erlang
RSAPrivateKeyPEM = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', RSAPrivateKey)]).
file:write_file("my.key", RSAPrivateKeyPEM).
```

```
-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA15Spy6QYxqlG25i1uygrU5AY3xTRX8Bq/9EspqCht9iAEUG5
...
1Q033aDFz7D6XO8yfl3bqNsk+BdpApPSqw3YrH5iYowkpeZ/2aR9rkI=
-----END RSA PRIVATE KEY-----
```

You can display the key with `openssl`:

```sh
openssl rsa -in my.key -text -noout
```

## Deriving the public key

For RSA, it's pretty simple; you just copy the modulus and exponent into an `RSAPublicKey` record:

```erlang
rr(public_key).   % if in the REPL
#'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = RSAPrivateKey.
RSAPublicKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent}.
```

## Writing the public key as PEM (PKCS#1)

```erlang
RSAPublicKeyPEM = public_key:pem_encode([public_key:pem_entry_encode('RSAPublicKey', RSAPublicKey)]).
file:write_file("my.pub", RSAPublicKeyPEM).
```

```
-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEA3hhgKg0aXwFMKe5dTTlmpnnQnLtUFy0iwwGFyjI8JdGtCgAMNoZB
...
IxbtysiR5Z8uES9jFMW5IMuciorjIhThuQIDAQAB
-----END RSA PUBLIC KEY-----
```

## Writing the public key as PEM (PKCS#8)

`openssl` -- as far as I can tell -- doesn't want to load public keys in this format, so we need to use PKCS#8 instead.
To do this, we wrap the key in a `SubjectPublicKeyInfo` record:

```erlang
SubjectPublicKeyInfo = #'SubjectPublicKeyInfo'{
  algorithm = #'AlgorithmIdentifier'{algorithm = {1,2,840,113549,1,1,1}, parameters = <<5,0>>},
  subjectPublicKey = public_key:der_encode('RSAPublicKey', RSAPublicKey)}.
Wrapped = public_key:pem_encode([public_key:pem_entry_encode('SubjectPublicKeyInfo', SubjectPublicKeyInfo)]).
file:write_file("my.pub", Wrapped).
```

The `{1,2,840,113549,1,1,1}` refers to the `rsaEncryption` OID; see <http://www.oid-info.com/get/1.2.840.113549.1.1.1>.
Normally, you'd use the `?'rsaEncryption'` macro, but that doesn't work in the REPL.

The `<<5,0>>` is a `NULL`; the algorithm takes no parameters.

And now we can load that with `openssl`:

```sh
openssl rsa -pubin -in wrapped.pem -text -noout
```
