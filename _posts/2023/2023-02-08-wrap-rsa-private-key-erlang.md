---
title: "Wrapping an RSA private key with Erlang"
date: 2023-02-08T11:13:00Z
tags: erlang cryptography
---

The previous post finished up by writing an RSA public key in "wrapped" (PKCS#8) PEM-format. It occurred to me that I should probably show how to do the same with the _private_ key as well.

We'll start with a private key:

```erlang
Size = 2048,  % in bits
Exp = 65537,  % standard RSA exponent
RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}).
```

Then we need to wrap it in a `PrivateKeyInfo` record:

```erlang
rr(public_key).   % if in the REPL, otherwise -include_lib("public_key/include/public_key.hrl").

PrivateKeyInfo = #'PrivateKeyInfo'{
  version = 'v1',
  privateKeyAlgorithm = #'PrivateKeyInfo_privateKeyAlgorithm'{
    algorithm = {1,2,840,113549,1,1,1},   % if in the REPL, otherwise ?'rsaEncryption'
    parameters = {'asn1_OPENTYPE', <<5,0>>}},
  privateKey = public_key:der_encode('RSAPrivateKey', RSAPrivateKey)}.

file:write_file("my.key",
  public_key:pem_encode([public_key:pem_entry_encode('PrivateKeyInfo', PrivateKeyInfo)])).
```

As before, the `{1,2,840,113549,1,1,1}` refers to the `rsaEncryption` OID; see
<http://www.oid-info.com/get/1.2.840.113549.1.1.1>. Normally, you'd use the `?'rsaEncryption'` macro, but that doesn't
work in the REPL.

The algorithm takes no parameters, but this time we need to wrap it in `asn1_OPENTYPE`, because ASN.1 reasons.

And now we can load that with `openssl`:

```sh
openssl rsa -in my.key -text -noout
```
