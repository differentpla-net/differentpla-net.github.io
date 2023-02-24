---
title: "Encrypting a JSON Web Token in Erlang, using JOSE"
date: 2023-02-13T17:46:00Z
tags: erlang cryptography jwt jose
---

In an earlier post, I showed how to [encrypt a JSON Web Token in Erlang]({% post_url
2023/2023-02-13-jwt-encryption-erlang %}). I used plain-ol' Erlang. Here's how to use the
[jose](https://hex.pm/packages/jose) package to do the same thing.

## Signed JWT

First, we'll create a signed JWT:

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
SigningJWK = jose_jwk:from_key(SigningKey).

JWT = #{<<"sub">> => <<"1234567890">>,
        <<"name">> => <<"John Doe">>,
        <<"iat">> => 1516239022}.
JWS = jose_jwt:sign(SigningJWK, #{<<"alg">> => <<"RS256">>}, JWT).
{_, CompactJWS} = jose_jws:compact(JWS).
```

## Encrypted JWT

Then we'll encrypt it; note that the sender uses the recipient's _public_ key:

```erlang
EncryptionKey = public_key:generate_key({rsa, 2048, 65537}).
EncryptionJWK = jose_jwk:to_public(jose_jwk:from_key(EncryptionKey)).   % public key

JWE = jose_jwk:block_encrypt(CompactJWS, EncryptionJWK).
{_, CompactJWE} = jose_jwe:compact(JWE).
```

## Decrypting an encrypted JWT

For symmetry, let's decrypt it and verify it as well.

Decryption uses the recipient's _private_ key:

```erlang
DecryptionJWK = jose_jwk:from_key(EncryptionKey).
{CompactJWS, _} = jose_jwk:block_decrypt(CompactJWE, DecryptionJWK).
```

## Verifying a signed JWT

Verification uses the sender's _public_ key:

```erlang
VerificationJWK = jose_jwk:to_public(jose_jwk:from_key(SigningKey)).   % public key
{true, {jose_jwt, JWT}, _} = jose_jwt:verify(VerificationJWK, CompactJWS).
```
