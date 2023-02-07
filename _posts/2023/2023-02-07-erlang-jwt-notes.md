---
title: "Erlang JWT Notes"
date: 2023-02-07T14:39:00Z
tags: erlang jwt
---

TODO: Regenerate the example data; it's a bit of a mishmash.

## Generating RSA keys in Erlang

```erlang
Size = 2048.
Exp = 65537.
RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}).
```

## Converting to PKCS#1

```erlang
PEM = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', RSAPrivateKey)]).
file:write_file("key.pem", PEM).
```

```
-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA0StApmitVQtpkCFdEr4+HDL04JV9TZylFF3ifwQcfD7jeai8
...
wlAFswzoRNZ1ip1jRewa4XTN7pZLvacKona0JIfGca3nT3JhaFewD9g=
-----END RSA PRIVATE KEY-----
```

## Display that with openssl

```
openssl rsa -in key.pem -text -noout
```

## Deriving the public key

For RSA, it's pretty simple:

```erlang
#'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = RSAPrivateKey.
RSAPublicKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent}.
```

## Converting that to PKCS#1

```erlang
PEM = public_key:pem_encode([public_key:pem_entry_encode('RSAPublicKey', RSAPublicKey)]).
file:write_file("pub.pem", PEM).
```

```
-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEA3hhgKg0aXwFMKe5dTTlmpnnQnLtUFy0iwwGFyjI8JdGtCgAMNoZB
...
IxbtysiR5Z8uES9jFMW5IMuciorjIhThuQIDAQAB
-----END RSA PUBLIC KEY-----
```

openssl doesn't want to display that; we probably need PKCS#8, which I think is this: https://github.com/voltone/x509/blob/33ddd879c3d04478776262ad43b076791bdb3198/lib/x509/public_key.ex#L52

## How are `kid` generated?

It's arbitrary, but:

- <https://stackoverflow.com/questions/55203195/generate-kid-for-jwt-from-public-key-of-signing-key>

The Google kid values are...?

- <https://accounts.google.com/.well-known/openid-configuration>
- <https://www.googleapis.com/oauth2/v3/certs>

```
kid: "274052a2b6448745764e72c3590971d90cfb585a"
n: "w0Pgy...W0NHQ"
```

```
N = <<"w0PgyEXUS2Stec6a5nxWPg_39M9D2x-zQedSwBEYthJ9d4x5mf-h69H2u555VYI6TUA59I0cyFlEKzqMsednebyfNBld1QCjb1q9xxnRSS4YrFiQSdXSPiurlrEvrl_O04pLLx_yoXnCRSgO_Q21wj0QsfNZ5quMIcr72kmswOiqCdZOWgWKkYt_UKJKEIYLkRNykGQeA6rBIomsTqKJzkBY4ke7YAoBS2BsQgmPgOGD39EGp2sqDvbcLYME-2z8HEMNZIL78sBnCQ0ov3Mv5F1ds8FcBUp1qWgG-j81HMN0SkZPK5RCteP4eacOaXYS7FzNyXQYi_45PBQi9W0NHQ">>.
<<"w0PgyEXUS2Stec6a5nxWPg_39M9D2x-zQedSwBEYthJ9d4x5mf-h69H2u555VYI6TUA59I0cyFlEKzqMsednebyfNBld1QCjb1q9xxnRSS4YrFiQSdXS"...>>
2> base64url:decode(N).
<<195,67,224,200,69,212,75,100,173,121,206,154,230,124,86,
  62,15,247,244,207,67,219,31,179,65,231,82,192,17,...>>
```

Can't find a hash that fits. The Google kid is 40 hex characters, so 160 bits. That's an odd hash length. But it's
obviously also not a UUID.

## Create JWT

```erlang
KeyId = base64url:encode(crypto:hash(sha256, base64url:decode(N))).
Header = base64url:encode(jsx:encode(#{<<"alg">> => <<"RS256">>, <<"kid">> => KeyId})).
```

Registered claims: https://www.rfc-editor.org/rfc/rfc7519.html#section-4.1

exp, nbf, iat are NumericDate -- unix epoch, seconds.

```erlang
Payload = base64url:encode(jsx:encode(#{
  <<"iss">> => Issuer,
  <<"sub">> => Subject,
  <<"aud">> => Audience,
  <<"exp">> => Expiry,
  <<"nbf">> => NotBefore,
  <<"iat">> => IssuedAt,
  <<"jti">> => UniqueId
})).
```



## TODO

- Create JWT.
- Sign JWT.
- Encrypt JWT.
- Compare with JOSE library.
