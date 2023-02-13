---
title: "Encrypting a JSON Web Token in Erlang"
date: 2023-02-13T15:37:00Z
tags: erlang cryptography jwt
---

Following on from [signing a web token in Erlang]({% post_url 2023/2023-02-09-jwt-signing-erlang %}), here's how to
encrypt one.

Let's recap the JWT signing steps, so we've got something to encrypt. Here's our token header and payload:

## Signing a JWT

```erlang
Header = base64url:encode(
    jsx:encode(
        #{<<"alg">> => <<"RS256">>,
          <<"typ">> => <<"JWT">>}
    )).
Payload = base64url:encode(
    jsx:encode(
        #{<<"sub">> => <<"1234567890">>,
          <<"name">> => <<"John Doe">>,
          <<"iat">> => 1516239022}
    )).
```

We can sign that as follows:

```erlang
SigningKey = public_key:generate_key({rsa, 2048, 65537}).
Signature = base64url:encode(
    public_key:sign(<<Header/binary, ".", Payload/binary>>, sha256, SigningKey)).
SignedJWT = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>.
```

## Encrypting a JWT

I'll use the same settings as the JOSE library defaults.

### Encryption Key

The signed token is encrypted using a symmetric key. The symmetric key is encrypted using the recipient's public key.
For this example, we'll generate one:

```erlang
rr(public_key).

EncryptionPrivateKey = public_key:generate_key({rsa, 2048, 65537}).
#'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = EncryptionPrivateKey.
EncryptionPublicKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent}.
```

### JWE Header

The JWE header contains the following parameters:

```erlang
JWEHeader = jsx:encode(
        #{<<"alg">> => <<"RSA-OAEP">>,
          <<"enc">> => <<"A128GCM">>}
    ).
```

Whether a parameter is required or optional varies by specific implementation. For example, some implementations require
a `"cty": "JWT"` header.

### Content Encryption Key (CEK)

The encryption algorithms are IANA-registered; see <https://www.iana.org/assignments/jose/jose.xhtml>. Looking up
`A128GCM` in the registry takes you to [RFC 7518, section 5.3](https://www.rfc-editor.org/rfc/rfc7518.html#section-5.3),
which states that `A128GCM` is "AES GCM using 128-bit key".

```erlang
CEK = crypto:strong_rand_bytes(16).
```

So that the recipient knows which key we used, it is encrypted using the recipient's public key. This is how you do
`RSA-OAEP` in Erlang:

```erlang
EncryptedCEK = public_key:encrypt_public(CEK, EncryptionPublicKey, [{rsa_padding, rsa_pkcs1_oaep_padding}]).
```

### Initialization Vector (IV)

AES GCM requires a 12 byte IV:

```erlang
IV = crypto:strong_rand_bytes(12).
```

### Additional Authenticated Data

JWE requires that we attach additional data to the message, identifying the algorithm and encryption used. It's
authenticated but not encrypted. That's our original JWE Header, base64url-encoded:

```erlang
AAD = base64url:encode(JWEHeader).
```

### Content Encryption

Then we encrypt the plaintext (the signed JWT) using the AES GCM algorithm. In Erlang, that looks like this:

```erlang
{Ciphertext, Tag} = crypto:crypto_one_time_aead(aes_gcm, CEK, IV, SignedJWT, AAD, true).
```

### Compact Format

We can then jam it all together:

```erlang
JWEHeader_ = base64url:encode(JWEHeader).
EncryptedCEK_ = base64url:encode(EncryptedCEK).
IV_ = base64url:encode(IV).
Ciphertext_ = base64url:encode(Ciphertext).
Tag_ = base64url:encode(Tag).
EncryptedJWT = <<JWEHeader_/binary, ".", EncryptedCEK_/binary, ".", IV_/binary, ".", Ciphertext_/binary, ".", Tag_/binary>>.
```
